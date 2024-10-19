use ustr::Ustr;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AstNodeId(u32);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident {
    pub id: AstNodeId,
    pub text: Ustr,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Yes,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Statement(Statement),
    Function(FunctionDef),
}
