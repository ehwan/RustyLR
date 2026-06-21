use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, PublishDiagnostics,
    },
    request::GotoDefinition,
    GotoDefinitionResponse, InitializeParams, Location, OneOf, PublishDiagnosticsParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use std::collections::HashMap;
use std::error::Error;

// Import the traits providing `METHOD` constant:
use lsp_types::notification::Notification as LspNotification;
use lsp_types::request::Request as LspRequest;

mod diagnostics;
mod goto_definition;
mod position;

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
    eprintln!("Starting RustyLR LSP server...");

    // Create stdio transport connection
    let (connection, io_threads) = Connection::stdio();

    // Advertise full document sync and definition provider capabilities
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })?;

    let initialization_params = connection.initialize(server_capabilities)?;
    let _params: InitializeParams = serde_json::from_value(initialization_params)?;

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
                        if let Some(range) = goto_definition::find_definition(content, position) {
                            let loc = Location::new(uri.clone(), range);
                            response = Response::new_ok(id, GotoDefinitionResponse::Scalar(loc));
                        }
                    }
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

fn publish_diagnostics(connection: &Connection, uri: Url, content: &str) {
    let diags = diagnostics::compile_and_get_diagnostics(content);
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics: diags,
        version: None,
    };
    let notification = Notification::new(PublishDiagnostics::METHOD.to_string(), params);
    let _ = connection.sender.send(Message::Notification(notification));
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
