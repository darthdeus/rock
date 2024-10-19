#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AstNodeId(u32);


#[derive(Debug, Clone)]
enum Statement {
    Yes,
}

#[derive(Debug, Clone)]
struct FunctionDef {}

#[derive(Debug, Clone)]
enum TopLevel {
    Statement(Statement),
    Function(FunctionDef),
}
