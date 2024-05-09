use std::{default, ffi::OsStr, path::PathBuf};

use ruparse::{lexer::PreprocessorError, parser::{ParseError, VariableKind}};
use runtime::Context;

mod dictionary;

const TEXT: &str = 
r##"import "#io"

use io.println;

fun main() {
    println("Hello, World!");
}
"##;

#[derive(Debug, Default)]
pub  struct Compiler {
    pub input: Input,
    pub output: Output,
    pub target: CompileTarget,
    pub libraries: Vec<Library>,
}

#[derive(Debug)]
pub struct Input {
    /// The source code of the program.
    /// The source code can be provided as a string or a file.
    pub source: &'static str,
    /// The kind of source code.
    pub source_kind: Sources,
    /// The directory where the source code is located.
    pub directory: &'static str,
}

impl Input {
    pub fn new(source: &'static str, source_kind: Sources, directory: &'static str) -> Self {
        Self {
            source,
            source_kind,
            directory,
        }
    }

    pub fn from_memory(source: &'static str) -> Self {
        Self {
            source,
            source_kind: Sources::Memory,
            directory: ".",
        }
    }

    pub fn from_file(source: &'static str, directory: &'static str) -> Self {
        Self {
            source,
            source_kind: Sources::File,
            directory,
        }
    }
}

#[derive(Debug)]
pub struct Output {
    /// The directory for compiled files and generated artifacts.
    pub directory: &'static str,
    /// Whether to generate artifacts in the output directory.
    pub gen_artifacts: bool,
    /// Whether to read artifacts from the output directory.
    pub read_artifacts: bool,
}

impl Output {
    pub fn new(directory: &'static str, gen_artifacts: bool, read_artifacts: bool) -> Self {
        Self {
            directory,
            gen_artifacts,
            read_artifacts,
        }
    }

    pub fn with_artifacts(directory: &'static str) -> Self {
        Self {
            directory,
            gen_artifacts: true,
            read_artifacts: true,
        }
    }

    pub fn no_artifacts(directory: &'static str) -> Self {
        Self {
            directory,
            gen_artifacts: false,
            read_artifacts: false,
        }
    }
}

#[derive(Debug)]
pub enum Sources {
    File,
    Memory,
}

impl default::Default for Input {
    fn default() -> Self {
        Self {
            source: "main.nrd",
            source_kind: Sources::File,
            directory: ".",
        }
    }
}

impl default::Default for Output {
    fn default() -> Self {
        Self {
            directory: "./target",
            gen_artifacts: false,
            read_artifacts: false,
        }
    }
}

#[derive(Debug, Default)]
pub enum CompileTarget {
    #[default]
    Executable,
    Library,
}

#[derive(Debug)]
pub struct Library {
    pub name: String,
    pub kind: LibraryKinds,
    pub path: String,
}

impl Library {
    pub fn new(path: &PathBuf) -> Library {
        let mut name = path.file_stem().unwrap_or(OsStr::new("")).to_str().unwrap().to_string();

        let kind = if path.is_file() {
            if path.extension().unwrap() == "nrd" {
                LibraryKinds::Source
            } else if path.extension().unwrap() == "nrl" {
                LibraryKinds::Native
            } else {
                LibraryKinds::Unknown
            }
        } else if path.is_dir(){
            LibraryKinds::Unknown
        } else if path.to_str().unwrap().starts_with("#"){
            name = name.strip_prefix("#").unwrap().to_string();
            LibraryKinds::RuntimeProvided
        } else {
            LibraryKinds::Notfound
        };

        let path = path.to_str().unwrap().to_string();

        Library {
            name,
            path,
            kind,
        }
    }

}

#[derive(Debug, Default, PartialEq)]
pub enum LibraryKinds {
    #[default]
    /// A source library is a Neruda source file that is compiled along with the main program.
    Source,

    /// A native library is a precompiled system library that is linked with the main program.
    Native,
    /// A runtime provided library is provided by the runtime.
    /// Neruda CLI provides standard libraries by default.
    RuntimeProvided,
    
    /// An unknown library is not recognized by the compiler.
    /// This could be a directory or a file that does not have a recognized extension.
    /// The user can provide flags to specify the kind of library.
    Unknown,
    /// A not found library is a library that does not exist.
    Notfound,
}

impl Compiler {
    pub fn compile(&self) -> Result<Context, CompileError> {
        let parser = neruda_ast::gen_parser();
        
        let tokens = parser.lexer.lex_utf8(&TEXT)?;
        let ast = parser.parse(&tokens, &TEXT)?;
    
        let imports = match ast.globals.get("imports") {
            Some(VariableKind::NodeList(imports)) => imports,
            _ => unreachable!(),
        };
        for node in imports {
            let string = neruda_ast::ast::read_string(node, &TEXT);
            println!("{}", string);
        }
    
        let mut context = Context::new();
    
        Ok(context)
    }
}

pub enum CompileError {
    LexerError(PreprocessorError),
    ParserError(ParseError),
}

impl From<PreprocessorError> for CompileError {
    fn from(err: PreprocessorError) -> CompileError {
        CompileError::LexerError(err)
    }
}

impl From<ParseError> for CompileError {
    fn from(err: ParseError) -> CompileError {
        CompileError::ParserError(err)
    }
}

impl std::fmt::Debug for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::LexerError(err) => write!(f, "LexerError: {:?}", err),
            CompileError::ParserError(err) => write!(f, "ParserError: {:?}", err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let compiler = Compiler {
            input: Input::from_memory(TEXT),
            output: Output::no_artifacts("./target"),
            target: CompileTarget::Executable,
            libraries: vec![
                Library::new(&PathBuf::from("#io")),
            ],
        };

        println!("{:#?}", compiler);

        let context = compiler.compile().unwrap();
        println!("{:#?}", context);

        panic!("all oogabooga!")
    }

    #[test]
    fn test_library() {
        let lib = Library::new(&PathBuf::from("tests/io.nrd"));
        assert_eq!(lib.name, "io");
        assert_eq!(lib.kind, LibraryKinds::Source);

        let lib = Library::new(&PathBuf::from("tests/io.nrl"));
        assert_eq!(lib.name, "io");
        assert_eq!(lib.kind, LibraryKinds::Native);

        let lib = Library::new(&PathBuf::from("tests/io"));
        assert_eq!(lib.name, "io");
        assert_eq!(lib.kind, LibraryKinds::Unknown);

        let lib = Library::new(&PathBuf::from("#io"));
        assert_eq!(lib.name, "io");
        assert_eq!(lib.kind, LibraryKinds::RuntimeProvided);

        let lib = Library::new(&PathBuf::from("tests/text.txt"));
        assert_eq!(lib.name, "text");
        assert_eq!(lib.kind, LibraryKinds::Unknown);

        let lib = Library::new(&PathBuf::from("tests/notfound"));
        assert_eq!(lib.name, "notfound");
        assert_eq!(lib.kind, LibraryKinds::Notfound);

        let lib = Library::new(&PathBuf::from(""));
        assert_eq!(lib.name, "");
        assert_eq!(lib.kind, LibraryKinds::Notfound);
    }
}
