use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;

#[derive(Clone, Debug)]
pub(crate) enum TermType {
    Ident(Option<proc_macro2::Ident>),
    Colon(Option<proc_macro2::Punct>),
    Semicolon(Option<proc_macro2::Punct>),
    Pipe(Option<proc_macro2::Punct>),
    Percent(Option<proc_macro2::Punct>),
    Left(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %left, %l, %reduce
    Right(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %right, %r, %shift
    Token(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %token
    Start(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %start
    EofDef(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %eof
    TokenType(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %tokentype
    UserData(Option<(proc_macro2::Punct, proc_macro2::Ident)>), // %userdata
    Group(Option<proc_macro2::Group>),
    Literal(Option<proc_macro2::Literal>),
    Equal(Option<proc_macro2::Punct>),
    OtherPunct(Option<proc_macro2::Punct>),
    Eof,
}
impl TermType {
    pub fn enum_index(&self) -> usize {
        match self {
            TermType::Ident(_) => 0,
            TermType::Colon(_) => 1,
            TermType::Semicolon(_) => 2,
            TermType::Pipe(_) => 3,
            TermType::Percent(_) => 4,
            TermType::Left(_) => 5,
            TermType::Right(_) => 6,
            TermType::Token(_) => 8,
            TermType::Start(_) => 9,
            TermType::EofDef(_) => 10,
            TermType::TokenType(_) => 11,
            TermType::UserData(_) => 12,
            TermType::Group(_) => 13,
            TermType::Literal(_) => 14,
            TermType::Equal(_) => 15,
            TermType::OtherPunct(_) => 16,
            TermType::Eof => 17,
        }
    }
    pub fn stream(self) -> TokenStream {
        match self {
            TermType::Ident(ident) => ident.unwrap().to_token_stream(),
            TermType::Colon(punct) => punct.unwrap().to_token_stream(),
            TermType::Semicolon(punct) => punct.unwrap().to_token_stream(),
            TermType::Pipe(punct) => punct.unwrap().to_token_stream(),
            TermType::Percent(punct) => punct.unwrap().to_token_stream(),
            TermType::Left(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Right(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Token(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Start(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::EofDef(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::TokenType(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::UserData(punct_ident) => {
                let (punct, ident) = punct_ident.unwrap();
                quote! { #punct #ident }
            }
            TermType::Group(group) => group.unwrap().to_token_stream(),
            TermType::Literal(lit) => lit.unwrap().to_token_stream(),
            TermType::Equal(punct) => punct.unwrap().to_token_stream(),
            TermType::OtherPunct(punct) => punct.unwrap().to_token_stream(),
            TermType::Eof => unreachable!("Eof should not be converted to TokenStream"),
        }
    }
    #[allow(unused_variables)]
    pub fn span(&self) -> Option<Span> {
        match self {
            TermType::Ident(ident) => ident.as_ref().map(|i| i.span()),
            TermType::Colon(punct) => punct.as_ref().map(|p| p.span()),
            TermType::Semicolon(punct) => punct.as_ref().map(|p| p.span()),
            TermType::Pipe(punct) => punct.as_ref().map(|p| p.span()),
            TermType::Percent(punct) => punct.as_ref().map(|p| p.span()),
            TermType::Left(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            TermType::Right(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            TermType::Token(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            TermType::Start(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            TermType::EofDef(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            TermType::TokenType(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            TermType::UserData(punct_ident) => punct_ident.as_ref().map(|(p, i)| i.span()),
            TermType::Group(group) => group.as_ref().map(|g| g.span()),
            TermType::Literal(lit) => lit.as_ref().map(|l| l.span()),
            TermType::Equal(punct) => punct.as_ref().map(|p| p.span()),
            TermType::OtherPunct(punct) => punct.as_ref().map(|p| p.span()),
            TermType::Eof => None,
        }
    }
}
impl std::fmt::Display for TermType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TermType::Ident(_) => write!(f, "TokenTree::Ident"),
            TermType::Colon(_) => write!(f, ":"),
            TermType::Semicolon(_) => write!(f, ";"),
            TermType::Pipe(_) => write!(f, "|"),
            TermType::Percent(_) => write!(f, "%"),
            TermType::Left(_) => write!(f, "%left"),
            TermType::Right(_) => write!(f, "%right"),
            TermType::Token(_) => write!(f, "%token"),
            TermType::Start(_) => write!(f, "%start"),
            TermType::EofDef(_) => write!(f, "%eof"),
            TermType::TokenType(_) => write!(f, "%tokentype"),
            TermType::UserData(_) => write!(f, "%userdata"),
            TermType::Group(_) => write!(f, "TokenTree::Group"),
            TermType::Literal(_) => write!(f, "TokenTree::Literal"),
            TermType::Equal(_) => write!(f, "="),
            TermType::OtherPunct(_) => write!(f, "TokenTree::Punct"),
            TermType::Eof => write!(f, "$"),
        }
    }
}
impl std::hash::Hash for TermType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.enum_index().hash(state);
    }
}
impl PartialEq for TermType {
    fn eq(&self, other: &Self) -> bool {
        self.enum_index() == other.enum_index()
    }
}
impl Eq for TermType {}
impl std::cmp::PartialOrd for TermType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for TermType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.enum_index().cmp(&other.enum_index())
    }
}
