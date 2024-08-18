use proc_macro2::TokenStream;

pub struct Output {
    /// token stream before '%%'
    pub user_stream: TokenStream,
    /// token stream after '%%'
    pub generated_stream: TokenStream,
    /// debug comments attatched to the output file
    pub debug_comments: String,
}
