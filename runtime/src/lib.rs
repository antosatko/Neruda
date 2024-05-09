use serde::{Deserialize, Serialize};

/// Context for Neruda VM
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Context {
    pub memory: Memory,
    pub executor: Executor,
}

impl Context {
    pub fn new() -> Context {
        Context {
            memory: Memory {
                stack: Stack {
                    data: vec![],
                    frames: vec![],
                    frame_pointer: 0,
                },
                heap: Heap { objects: vec![] },
                strings: Strings { data: vec![] },
                module: Module {
                    functions: vec![],
                    classes: vec![],
                    closures: vec![],
                },
            },
            executor: Executor {
                instructions: vec![],
                instruction_pointer: 0,
            },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Memory {
    pub stack: Stack,
    pub heap: Heap,
    pub strings: Strings,
    pub module: Module,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Stack {
    pub data: Vec<Value>,
    pub frames: Vec<StackFrame>,
    pub frame_pointer: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct StackFrame {
    pub start: u64,
    pub end: u64,
    pub function_id: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Heap {
    pub objects: Vec<Object>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Object {
    pub id: u64,
    pub kind: ObjectKind,
    pub data: Vec<Value>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ObjectKind {
    Class = 0,
    Array = 1,
    Tuple = 2,
    Singleton = 3,

    Unknown = 255,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Strings {
    pub data: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Executor {
    pub instructions: Vec<Instructions>,
    pub instruction_pointer: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Function {
    /// Unique identifier for the function
    ///
    /// This is also index of the function in the
    /// function table.
    pub id: u64,
    pub name: String,
    pub position: u64,
    pub stack_size: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Class {
    /// Unique identifier for the class
    ///
    /// This is also index of the class in the
    /// class table.
    pub id: u64,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Closure {
    /// Unique identifier for the closure
    ///
    /// This is also index of the closure in the
    /// closure table.
    pub id: u64,
    pub position: u64,
    pub stack_size: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Module {
    pub functions: Vec<Function>,
    pub classes: Vec<Class>,
    pub closures: Vec<Closure>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Instructions {}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Value {
    // -- Primitive types --
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    Uint(u64),

    // -- Pointer types --
    ObjectPtr(u64),
    HeapPtr {
        obj: u64,
        offset: u64,
    },
    StackPtr(u64),
    StringPtr(u64),
    CharPtr {
        obj: u64,
        offset: u64,
    },
    UserdataPtr(u64),

    // -- Identifier types --
    FunctionIdent(u64),
    ClassIdent(u64),
    ClosureIdent {
        /// Unique identifier for the closure
        id: u64,
        /// Location of the closure on the heap
        captured: u64,
    },

    // -- Special types --
    Void,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ValueKind {
    Primitive,
    Pointer,
    Identifier,
    Void,
}

impl Value {
    pub fn is_primitive(&self) -> bool {
        match self {
            Value::Int(_) => true,
            Value::Float(_) => true,
            Value::Char(_) => true,
            Value::Bool(_) => true,
            Value::Uint(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Value::ObjectPtr(_) => true,
            Value::HeapPtr { .. } => true,
            Value::StackPtr(_) => true,
            Value::StringPtr(_) => true,
            Value::CharPtr { .. } => true,
            Value::UserdataPtr(_) => true,
            _ => false,
        }
    }

    pub fn is_identifier(&self) -> bool {
        match self {
            Value::FunctionIdent(_) => true,
            Value::ClassIdent(_) => true,
            Value::ClosureIdent { .. } => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Value::Void => true,
            _ => false,
        }
    }

    pub fn kind(&self) -> ValueKind {
        match self {
            Value::Int(_) => ValueKind::Primitive,
            Value::Float(_) => ValueKind::Primitive,
            Value::Char(_) => ValueKind::Primitive,
            Value::Bool(_) => ValueKind::Primitive,
            Value::Uint(_) => ValueKind::Primitive,

            Value::ObjectPtr(_) => ValueKind::Pointer,
            Value::HeapPtr { .. } => ValueKind::Pointer,
            Value::StackPtr(_) => ValueKind::Pointer,
            Value::StringPtr(_) => ValueKind::Pointer,
            Value::CharPtr { .. } => ValueKind::Pointer,
            Value::UserdataPtr(_) => ValueKind::Pointer,

            Value::FunctionIdent(_) => ValueKind::Identifier,
            Value::ClassIdent(_) => ValueKind::Identifier,
            Value::ClosureIdent { .. } => ValueKind::Identifier,

            Value::Void => ValueKind::Void,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
