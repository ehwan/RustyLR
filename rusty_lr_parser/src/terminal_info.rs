use proc_macro2::Ident;
use proc_macro2::Span;

use proc_macro2::TokenStream;
use rusty_lr_core::ReduceType;

pub struct ReduceTypeInfo {
    pub reduce_type: ReduceType,

    // the span of this reduce type definition
    pub source: Span,
}
pub struct TerminalInfo {
    pub name: Ident,

    /// any %left of %right set for this terminal
    pub reduce_type: Option<ReduceTypeInfo>,

    /// the actual Rust expr to be emitted
    pub body: TokenStream,
}
