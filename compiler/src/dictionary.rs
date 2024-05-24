use std::collections::HashMap;

use ruparse::parser::ParseResult;

pub struct Line {
    pub column: u32,
    pub line: u32,
    pub file: String,
}

pub struct Dictionary {}

pub struct Symbol {
    pub docs: Option<String>,
    pub path: SymbolPath,
    pub access: AccessModifier,
    pub kind: Symbols,
    pub line: Line,
}

pub enum Symbols {}

impl Symbol {
    pub fn new(
        path: SymbolPath,
        access: AccessModifier,
        kind: Symbols,
        line: Line,
        docs: Option<String>,
    ) -> Self {
        Self {
            docs,
            path,
            access,
            kind,
            line,
        }
    }

    pub fn accesible_from(&self, from: &SymbolPath) -> bool {
        match self.access {
            AccessModifier::Public => true,
            AccessModifier::Private => match self.path.compare(from) {
                PathComparison::Equal => true,
                PathComparison::IdentMismatch => true,
                _ => false,
            },
            AccessModifier::File => match self.path.compare(from) {
                PathComparison::Equal => true,
                PathComparison::IdentMismatch => true,
                PathComparison::ModuleMismatch => true,
                _ => false,
            },
        }
    }
}

pub struct Type {
    pub kind: Types,
    pub generics: Vec<String>,
    pub reference: i8,
    pub line: Line,
}

pub enum PrimitiveTypes {
    Int,
    Float,
    Char,
    Bool,
    String,
    Null,
}

pub enum Types {
    Primitive {
        kind: PrimitiveTypes,
    },

    Array {
        inner: Box<Types>,
    },
    Tuple {
        inner: Vec<Types>,
    },

    Struct {
        path: SymbolPath,
    },
    Enum {
        path: SymbolPath,
    },

    Function {
        path: SymbolPath,
        generics: Vec<GenericDeclaration>,
        args: Vec<Argument>,
        return_type: Box<Types>,
    },

    /// A generic type
    /// refers to a type that is defined in function or class generics
    /// e.g. `T` in `class Foo<T> { ... }`
    /// or `T` in `fn foo<T>() { ... }`
    Generic {
        constraints: Vec<SymbolPath>,
    },

    Void,
}

pub struct Function {
    pub path: SymbolPath,
    pub access: AccessModifier,
    pub line: Line,
    pub docs: Option<String>,
    pub generics: Vec<GenericDeclaration>,
    pub args: Vec<Argument>,
    pub return_type: Types,
}

pub struct GenericDeclaration {
    pub identifier: String,
    pub constraints: Vec<SymbolPath>,
    pub line: Line,
}

pub struct Argument {
    pub identifier: ArgumentIdentifier,
    pub kind: Types,
    pub line: Line,
}

pub enum ArgumentIdentifier {
    Identifier(String),
    Tuple(Vec<ArgumentIdentifier>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccessModifier {
    /// Public symbols are accesible from any other module
    Public,
    /// Private symbols are only accesible from the same module
    Private,
    /// File symbols are only accesible from the same file
    File,
}

pub struct SymbolPath {
    pub file: String,
    pub module: String,
    pub identifier: String,
}

impl SymbolPath {
    pub fn new(file: String, module: String, identifier: String) -> Self {
        Self {
            file,
            module,
            identifier,
        }
    }

    pub fn compare(&self, other: &SymbolPath) -> PathComparison {
        if self.file == other.file {
            if self.module == other.module {
                if self.identifier == other.identifier {
                    PathComparison::Equal
                } else {
                    PathComparison::IdentMismatch
                }
            } else {
                PathComparison::ModuleMismatch
            }
        } else {
            PathComparison::FileMismatch
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathComparison {
    /// The two paths are the same
    Equal,
    /// The paths have the same file and module, but different identifiers
    IdentMismatch,
    /// The paths have the same file, but different modules
    ModuleMismatch,
    /// The paths have different files
    FileMismatch,
}

impl Dictionary {
    pub fn new() -> Self {
        Self {}
    }

    pub fn from_ast(ast: &ParseResult) -> Self {
        let this = Self::new();

        this
    }
}
