use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, PublishDiagnostics,
    },
    request::{CodeActionRequest, Completion, GotoDefinition, HoverRequest, InlayHintRequest},
    CodeActionKind, CodeActionOptions, CompletionOptions, Diagnostic, DiagnosticSeverity,
    GotoDefinitionResponse, Hover, HoverProviderCapability, InlayHint, InlayHintOptions,
    InlayHintServerCapabilities, Location, OneOf, PublishDiagnosticsParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use std::collections::HashMap;
use std::error::Error;
use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};

// Import the traits providing `METHOD` constant:
use lsp_types::notification::Notification as LspNotification;
use lsp_types::request::Request as LspRequest;

mod code_action;
mod completion;
mod diagnostics;
mod goto_definition;
mod hover;
mod inlay_hint;
mod position;

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
    eprintln!("Starting RustyLR LSP server...");

    // Create stdio transport connection
    let (connection, io_threads) = Connection::stdio();

    // Advertise full document sync and definition provider capabilities
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(lsp_types::CodeActionProviderCapability::Options(
            CodeActionOptions {
                code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
                resolve_provider: Some(false),
                ..Default::default()
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                resolve_provider: Some(false),
                ..Default::default()
            },
        ))),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(completion_trigger_characters()),
            ..Default::default()
        }),
        ..Default::default()
    })?;

    connection.initialize(server_capabilities)?;

    eprintln!("RustyLR LSP server initialized successfully.");

    // Store open document contents
    let mut documents: HashMap<Url, String> = HashMap::new();

    // Main event loop
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                if req.method == GotoDefinition::METHOD {
                    let (id, params) = match cast_request::<GotoDefinition>(req) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting goto definition request: {:?}", e);
                            continue;
                        }
                    };

                    let uri = params.text_document_position_params.text_document.uri;
                    let position = params.text_document_position_params.position;

                    let mut response = Response::new_ok(id.clone(), serde_json::Value::Null);
                    if let Some(content) = documents.get(&uri) {
                        match catch_lsp_panic(|| {
                            goto_definition::find_definition(content, position)
                        }) {
                            Ok(Some(range)) => {
                                let loc = Location::new(uri.clone(), range);
                                response =
                                    Response::new_ok(id, GotoDefinitionResponse::Scalar(loc));
                            }
                            Ok(None) => {}
                            Err(message) => {
                                eprintln!("RustyLR goto-definition panicked: {message}");
                            }
                        }
                    }
                    connection.sender.send(Message::Response(response))?;
                } else if req.method == Completion::METHOD {
                    let (id, params) = match cast_request::<Completion>(req) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting completion request: {:?}", e);
                            continue;
                        }
                    };

                    let uri = params.text_document_position.text_document.uri;
                    let position = params.text_document_position.position;
                    let response = if let Some(content) = documents.get(&uri) {
                        match catch_lsp_panic(|| completion::completions(content, position)) {
                            Ok(completions) => Response::new_ok(id, completions),
                            Err(message) => {
                                eprintln!("RustyLR completion panicked: {message}");
                                Response::new_ok(
                                    id,
                                    lsp_types::CompletionResponse::Array(Vec::new()),
                                )
                            }
                        }
                    } else {
                        Response::new_ok(id, lsp_types::CompletionResponse::Array(Vec::new()))
                    };
                    connection.sender.send(Message::Response(response))?;
                } else if req.method == HoverRequest::METHOD {
                    let (id, params) = match cast_request::<HoverRequest>(req) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting hover request: {:?}", e);
                            continue;
                        }
                    };

                    let uri = params.text_document_position_params.text_document.uri;
                    let position = params.text_document_position_params.position;
                    let response = if let Some(content) = documents.get(&uri) {
                        match catch_lsp_panic(|| hover::hover(content, position)) {
                            Ok(hover) => Response::new_ok(id, hover),
                            Err(message) => {
                                eprintln!("RustyLR hover panicked: {message}");
                                Response::new_ok(id, Option::<Hover>::None)
                            }
                        }
                    } else {
                        Response::new_ok(id, Option::<Hover>::None)
                    };
                    connection.sender.send(Message::Response(response))?;
                } else if req.method == InlayHintRequest::METHOD {
                    let (id, params) = match cast_request::<InlayHintRequest>(req) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting inlay hint request: {:?}", e);
                            continue;
                        }
                    };

                    let uri = params.text_document.uri;
                    let range = params.range;
                    let response = if let Some(content) = documents.get(&uri) {
                        match catch_lsp_panic(|| inlay_hint::inlay_hints(content, range)) {
                            Ok(hints) => Response::new_ok(id, Some(hints)),
                            Err(message) => {
                                eprintln!("RustyLR inlay hint panicked: {message}");
                                Response::new_ok(id, Option::<Vec<InlayHint>>::None)
                            }
                        }
                    } else {
                        Response::new_ok(id, Option::<Vec<InlayHint>>::None)
                    };
                    connection.sender.send(Message::Response(response))?;
                } else if req.method == CodeActionRequest::METHOD {
                    let (id, params) = match cast_request::<CodeActionRequest>(req) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting code action request: {:?}", e);
                            continue;
                        }
                    };

                    let uri = params.text_document.uri;
                    let response = if let Some(content) = documents.get(&uri) {
                        match catch_lsp_panic(|| {
                            code_action::code_actions(content, uri, params.context.diagnostics)
                        }) {
                            Ok(actions) => Response::new_ok(id, Some(actions)),
                            Err(message) => {
                                eprintln!("RustyLR code action panicked: {message}");
                                Response::new_ok(id, Option::<lsp_types::CodeActionResponse>::None)
                            }
                        }
                    } else {
                        Response::new_ok(id, Option::<lsp_types::CodeActionResponse>::None)
                    };
                    connection.sender.send(Message::Response(response))?;
                }
            }
            Message::Response(_resp) => {}
            Message::Notification(not) => {
                if not.method == DidOpenTextDocument::METHOD {
                    let params = match cast_notification::<DidOpenTextDocument>(not) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting didOpen notification: {:?}", e);
                            continue;
                        }
                    };
                    let uri = params.text_document.uri;
                    let text = params.text_document.text;

                    documents.insert(uri.clone(), text.clone());
                    publish_diagnostics(&connection, uri, &text);
                } else if not.method == DidChangeTextDocument::METHOD {
                    let params = match cast_notification::<DidChangeTextDocument>(not) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting didChange notification: {:?}", e);
                            continue;
                        }
                    };
                    let uri = params.text_document.uri;

                    if let Some(change) = params.content_changes.into_iter().next() {
                        documents.insert(uri.clone(), change.text.clone());
                        publish_diagnostics(&connection, uri, &change.text);
                    }
                } else if not.method == DidSaveTextDocument::METHOD {
                    let params = match cast_notification::<DidSaveTextDocument>(not) {
                        Ok(res) => res,
                        Err(e) => {
                            eprintln!("Error extracting didSave notification: {:?}", e);
                            continue;
                        }
                    };
                    let uri = params.text_document.uri;
                    if let Some(text) = documents.get(&uri) {
                        publish_diagnostics(&connection, uri, text);
                    }
                }
            }
        }
    }

    io_threads.join()?;
    eprintln!("RustyLR LSP server stopped.");
    Ok(())
}

fn completion_trigger_characters() -> Vec<String> {
    "%@$_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        .chars()
        .map(|ch| ch.to_string())
        .collect()
}

fn publish_diagnostics(connection: &Connection, uri: Url, content: &str) {
    let diags = match catch_lsp_panic(|| diagnostics::compile_and_get_diagnostics(content)) {
        Ok(diags) => diags,
        Err(message) => vec![Diagnostic {
            range: Range::default(),
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("rusty_lr".to_string()),
            message: format!("RustyLR compiler panicked: {message}"),
            related_information: None,
            tags: None,
            data: None,
        }],
    };
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics: diags,
        version: None,
    };
    let notification = Notification::new(PublishDiagnostics::METHOD.to_string(), params);
    let _ = connection.sender.send(Message::Notification(notification));
}

fn catch_lsp_panic<T>(f: impl FnOnce() -> T) -> Result<T, String> {
    let hook = take_hook();
    set_hook(Box::new(|_| {}));
    let result = catch_unwind(AssertUnwindSafe(f)).map_err(panic_message);
    set_hook(hook);
    result
}

fn panic_message(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "unknown panic payload".to_string()
    }
}

fn cast_request<R>(
    req: Request,
) -> Result<(RequestId, R::Params), lsp_server::ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notification<N>(
    not: Notification,
) -> Result<N::Params, lsp_server::ExtractError<Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}
