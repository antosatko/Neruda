use memory::*;
use module::*;
use std::sync::Arc;

/// Hold necessary data for starting the main thread
///
/// After the main thread is created, this struct will be consumed
#[derive(Debug, Default)]
pub struct Context {
    /// default memory for the thread
    pub memory: Memory,
    /// instructions for the thread
    pub instructions: Vec<Instructions>,
    /// entry point for the main thread
    pub entry_instruction: usize,
    /// module for the thread with descriptions of functions, classes, etc.
    pub module: Module,
}

/// A single thread of execution
#[derive(Debug)]
pub struct Thread {
    pub memory: Memory,
    pub instr_ptr: usize,
    pub stack_frames: StackFrames,
    pub ctx: Arc<Context>,
}

#[derive(Debug, Clone)]
pub struct StackFrames {
    /// stack frames
    pub frames: Vec<StackFrame>,
    /// next frame to be opened
    pub next: StackFrame,
}

impl StackFrames {
    pub fn new() -> Self {
        Self {
            frames: Vec::with_capacity(256),
            next: StackFrame {
                block: 0,
                return_value: 0,
                return_addr: 0,
                function: 0
            }
        }
    }

    pub fn push(&mut self, frame: StackFrame) {
        self.frames.push(frame);
    }

    pub fn pop(&mut self) -> Option<StackFrame> {
        self.frames.pop()
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub block: usize,
    pub return_value: StackAddr,
    pub return_addr: InstrAddr,
    pub function: ID,
}

/// an address in the stack
pub type StackAddr = usize;
/// a value in the stack
pub type StackValue = usize;
/// an address in the instruction memory
pub type InstrAddr = usize;
/// any ID
pub type ID = usize;

#[derive(Debug, Clone)]
pub enum Instructions {
    /// Prints the value in stack[<addr>] in debug mode
    Debug { addr: StackAddr },
    /// Exits the thread with the value in stack[<addr>]
    End { exit_value: StackAddr },
    /// No operation
    Noop,

    /// Loads <value> into the stack[<addr>]
    Load { value: Value, addr: StackAddr },
    /// Creates new copy of string in module.strings[<str>] and loads the pointer to stack[<addr>]
    LoadString { str: usize, addr: StackAddr },
    /// Moves the value from stack[<from>] to stack[<to>]
    Move { from: StackAddr, to: StackAddr },
    /// Swaps the values in stack[<addr1>] and stack[<addr2>]
    Swap { addr1: StackAddr, addr2: StackAddr },

    /// Goes to the instruction at <addr>
    Goto { addr: InstrAddr },
    /// Goes to the instruction at <addr> if the value in stack[<cond>] is true else goes to the instruction at <else_>
    Branch { cond: StackAddr, addr: InstrAddr, else_: InstrAddr },

    /// Adds the values in stack[<addr1>] and stack[<addr2>] and stores the result in stack[<addr3>]
    Add { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Subtracts the value in stack[<addr2>] from the value in stack[<addr1>] and stores the result in stack[<addr3>]
    Sub { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Multiplies the values in stack[<addr1>] and stack[<addr2>] and stores the result in stack[<addr3>]
    Mul { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Divides the value in stack[<addr1>] by the value in stack[<addr2>] and stores the result in stack[<addr3>]
    Div { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Modulus of the value in stack[<addr1>] by the value in stack[<addr2>] and stores the result in stack[<addr3>]
    Mod { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },

    /// Compares for stack[<addr1>] equal to stack[<addr2>] and stores the result in stack[<addr3>]
    Eq { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Compares for stack[<addr1>] greater than stack[<addr2>] and stores the result in stack[<addr3>]
    Gt { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Compares for stack[<addr1>] less than stack[<addr2>] and stores the result in stack[<addr3>]
    Lt { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Compares for stack[<addr1>] greater than or equal to stack[<addr2>] and stores the result in stack[<addr3>]
    Gteq { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Compares for stack[<addr1>] less than or equal to stack[<addr2>] and stores the result in stack[<addr3>]
    Lteq { addr1: StackAddr, addr2: StackAddr, addr3: StackAddr },
    /// Negates the value in stack[<addr1>] and stores the result in stack[<addr2>]
    Not { addr1: StackAddr, addr2: StackAddr },

    /// Opens a new stack frame for the function at <id> and stores the return address in stack[<addr>]
    Open { function: ID, addr: StackAddr },
    /// Copies the value in stack[<addr>] to the new stack frame
    Arg { addr: StackAddr, to: StackAddr },
    /// Jumps to the start of the function that has been opened
    Jump,
    /// Closes the current stack frame, copies the value in stack[<addr>] to the return value of the previous stack frame, and goes to the return address
    Return { addr: StackAddr },

}

pub mod module {
    use crate::api::NativeLib;

    /// Module contains definitions for functions, classes, closures, arrays, tuples, and strings
    #[derive(Debug, Clone, Default)]
    pub struct Module {
        pub functions: Vec<Function>,
        pub classes: Vec<Class>,
        pub closures: Vec<Closure>,
        pub arrays: Vec<Array>,
        pub tuples: Vec<Tuple>,
        pub strings: Vec<String>,
        pub native_libs: Vec<NativeLib>,
    }

    #[derive(Debug, Clone)]
    pub struct Line {
        pub line: usize,
        pub column: usize,
        pub file: String,
    }

    #[derive(Debug, Clone)]
    pub struct Type {
        pub kind: Types,
        pub refs: u64,
        pub line: Line,
    }

    impl std::fmt::Display for Type {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for _ in 0..self.refs {
                write!(f, "&")?;
            }
            write!(f, "{}", self.kind)
        }
    }

    #[derive(Debug, Clone)]
    pub enum Types {
        Word(String),
        Array(Box<Type>),
        Tuple(Vec<Type>),
        Function(Vec<(String, Type)>, Box<Type>),
    }

    impl std::fmt::Display for Types {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Types::Word(word) => write!(f, "{}", word),
                Types::Array(type_) => write!(f, "[{}]", type_),
                Types::Tuple(types) => {
                    write!(f, "(")?;
                    for (i, type_) in types.iter().enumerate() {
                        write!(f, "{}", type_)?;
                        if i != types.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                }
                Types::Function(args, ret) => {
                    write!(f, "fun(")?;
                    for (i, (name, type_)) in args.iter().enumerate() {
                        write!(f, "{}: {}", name, type_)?;
                        if i != args.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ") -> {}", ret)
                }
            }
        }
    }

    /// Function definition
    #[derive(Debug, Clone)]
    pub struct Function {
        pub name: String,
        pub args: Vec<(String, Type)>,
        pub ret: Type,
        /// The index of the first instruction in the function
        pub start: usize,
        /// The index of the last instruction in the function
        pub end: usize,
        pub line: Line,
    }

    /// Class definition
    #[derive(Debug, Clone)]
    pub struct Class {
        pub name: String,
        pub fields: Vec<(String, Type)>,
        pub methods: Vec<Function>,
        pub line: Line,
    }

    /// Closure definition
    #[derive(Debug, Clone)]
    pub struct Closure {
        pub name: String,
        pub args: Vec<(String, Type)>,
        pub start: usize,
        pub end: usize,
        pub line: Line,
    }

    /// Array definition
    #[derive(Debug, Clone)]
    pub struct Array {
        pub type_: Type,
        pub line: Line,
    }

    /// Tuple definition
    #[derive(Debug, Clone)]
    pub struct Tuple {
        pub types: Vec<Type>,
        pub line: Line,
    }
}

pub mod memory {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
    pub enum Value {
        Int(i64),
        Uint(u64),
        Float(f64),
        Char(char),
        Bool(bool),
        Null,

        Pointer { block: usize, offset: usize },
        Block { block: usize },

        String { str: usize },
        CharPtr { str: usize, offset: usize },
        Userdata { data: usize },

        Void,
    }

    pub trait Allocator<T> {
        fn allocate(&mut self, obj: T) -> usize;
        fn deallocate(&mut self, index: usize);
        fn realloc(&mut self, index: usize, size: usize);
    }

    /// Memory for a running thread
    #[derive(Debug, Default)]
    pub struct Memory {
        pub blocks: Blocks,
        pub strings: Strings,
        pub userdata: UDHeap,
    }

    /// Main memory structure
    ///
    /// Allocates block of memory
    #[derive(Debug, Clone, Default)]
    pub struct Blocks {
        pub blocks: Vec<Block>,
        /// Blocks that are free to use + their size
        pub free: Vec<(usize, usize)>,
    }

    #[derive(Debug, Clone, Default)]
    pub struct Strings {
        pub data: Vec<StringObject>,
        pub free: Vec<usize>,
    }

    #[derive(Debug, Default)]
    pub struct UDHeap {
        pub data: Vec<UDContainer>,
        pub free: Vec<usize>,
    }

    /// A block of memory
    #[derive(Debug, Clone)]
    pub struct Block {
        /// Data stored in the block
        pub data: Vec<Value>,
        /// Block that is free to use
        pub free: bool,
        /// Reference count
        pub count: u64,
    }

    #[derive(Debug, Clone)]
    pub struct StringObject {
        pub data: String,
        pub free: bool,
    }

    pub struct UDContainer {
        pub data: Box<dyn UserData>,
        pub free: bool,
    }

    impl std::fmt::Debug for UDContainer {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("UserData")
                .field("data", &self.data.label())
                .field("free", &self.free)
                .finish()
        }
    }

    pub trait UserData: Send + Sync {
        /// Get the label of the object
        ///
        /// This will be used for debugging purposes
        fn label(&self) -> &'static str;

        /// Clone the data
        ///
        /// This will only be called when opening a new thread
        ///
        /// If the data is not clonable, or can not be sent to another thread, return
        /// a new instance of the data and make sure to add this detail in the library
        /// documentation
        fn clone(&self) -> Box<dyn UserData>;

        /// Get the garbage collection method
        fn gc_method(&self) -> GCMethod {
            GCMethod::GC
        }

        /// Prepare the data for garbage collection
        fn collect(&mut self);

        /// Get the data as an `Any` trait object
        ///
        /// This will be used for downcasting the data
        ///
        /// # Example
        ///
        /// ```
        /// use std::any::Any;
        ///
        /// let data: Box<dyn Any> = Box::new(5);
        ///
        /// let data = data.downcast_ref::<i32>().unwrap();
        ///
        /// assert_eq!(*data, 5);
        /// ```
        fn as_any(&self) -> &dyn std::any::Any;

        /// Get the data as a mutable `Any` trait object
        ///
        /// This will be used for downcasting the data
        ///
        /// # Example
        ///
        /// ```
        /// use std::any::Any;
        ///
        /// let mut data: Box<dyn Any> = Box::new(5);
        ///
        /// let data = data.downcast_mut::<i32>().unwrap();
        ///
        /// assert_eq!(*data, 5);
        /// ```
        fn as_any_mut(&mut self) -> &mut dyn std::any::Any;
    }

    /// Explains to the garbage collector how to handle the data
    pub enum GCMethod {
        /// The data is not garbage collected
        None,
        /// The data is garbage collected
        GC,
        /// The data is garbage collected and the garbage collector will call the `collect` method
        Prepare,
    }
    
    impl Memory {
        pub fn get_value(&self, stack_block: usize, addr: usize) -> Value {
            self.blocks.blocks[stack_block].data[addr]
        }
    
        pub fn set_value(&mut self, stack_block: usize, addr: usize, value: Value) {
            self.blocks.blocks[stack_block].data[addr] = value;
        }
        }
}

pub mod api {
    use super::memory::Value;
    use crate::Thread;


    /// Native library function
    /// 
    /// This function will be called when a thread calls a native library function
    /// 
    /// # Arguments
    /// 
    /// - `th` - The thread that called the function
    /// - `id` - The id of the function that was called
    /// 
    /// # Returns
    /// 
    /// The return value of the function
    /// 
    /// # Errors
    /// 
    /// If the function does not exist, return `NativeLibErr::NotFound`
    /// 
    /// # Safety
    /// 
    /// This function should never panic. If an error occurs, return `NativeLibErr::Error`
    /// adn let the runtime handle the error
    pub type NativeLib = fn(th: &mut Thread, id: usize) -> Result<Value, NativeLibErr>;

    /// Error that can occur when calling a native library function
    #[derive(Debug)]
    pub enum NativeLibErr {
        /// The function does not exist
        NotFound,
        /// The function is not implemented
        NotImplemented,
        /// The function returned an error
        Error(String),
    }

    /// Initialize the native library
    /// 
    /// This function will be called when the library is loaded
    /// 
    /// # Example
    /// 
    /// ```
    /// //! Library that always returns 5
    /// use runtime::api::NativeLib;
    /// use runtime::memory::Value;
    /// 
    /// #[no_mangle]
    /// pub fn init() -> NativeLib {
    ///    |_, _| Ok(Value::Int(5))
    /// }
    /// ```
    pub type Init = fn() -> NativeLib;
}

impl Context {
    pub fn create_thread(self) -> Thread {
        Thread {
            memory: Memory::default(), // fixme: copy important memory data from context
            instr_ptr: self.entry_instruction,
            stack_frames: StackFrames::new(),
            ctx: Arc::new(self),
        }
    }
}

impl Thread {
    pub fn copy_new(&self) -> Self {
        Self {
            memory: Memory::default(),
            instr_ptr: 0,
            stack_frames: StackFrames::new(),
            ctx: self.ctx.clone(),
        }
    }

    pub fn run(&mut self) -> Value {
        let mut stack_block = self.stack_frames.frames.last().unwrap().block;
        loop {
            let instr = &self.ctx.instructions[self.instr_ptr];
            match instr {
                Instructions::Debug { addr } => {
                    let value = self.memory.get_value(stack_block, *addr);
                    println!("{:?}", value);
                    self.next_instr();
                }
                Instructions::End { exit_value } => {
                    let value = self.memory.get_value(stack_block, *exit_value);
                    return value;
                }
                Instructions::Noop => self.next_instr(),
                Instructions::Load { value, addr } => {
                    self.memory.set_value(stack_block, *addr, *value);
                    self.next_instr();
                }
                Instructions::LoadString { str, addr } => todo!(),
                Instructions::Move { from, to } => {
                    let value = self.memory.get_value(stack_block, *from);
                    self.memory.set_value(stack_block, *to, value);
                    self.next_instr();
                }
                Instructions::Swap { addr1, addr2 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    self.memory.set_value(stack_block, *addr1, value2);
                    self.memory.set_value(stack_block, *addr2, value1);
                    self.next_instr();
                }
                Instructions::Goto { addr } => {
                    self.instr_ptr = *addr;
                }
                Instructions::Branch { cond, addr, else_ } => {
                    let value = self.memory.get_value(stack_block, *cond);
                    if let Value::Bool(true) = value {
                        self.instr_ptr = *addr;
                    } else {
                        self.instr_ptr = *else_;
                    }
                }
                Instructions::Add { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a + b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                        (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 + b as u8) as char),
                        _ => panic!("Invalid types for addition"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Sub { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a - b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                        (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 - b as u8) as char),
                        _ => panic!("Invalid types for subtraction"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Mul { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a * b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                        (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 * b as u8) as char),
                        _ => panic!("Invalid types for multiplication"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Div { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a / b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                        (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 / b as u8) as char),
                        _ => panic!("Invalid types for division"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Mod { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a % b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
                        (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 % b as u8) as char),
                        _ => panic!("Invalid types for modulus"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Eq { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a == b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a == b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a == b),
                        (Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                        _ => panic!("Invalid types for equality"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Gt { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a > b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a > b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a as u8 > b as u8),
                        _ => panic!("Invalid types for greater than"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Lt { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a < b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a < b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool((a as u8) < b as u8),
                        _ => panic!("Invalid types for less than"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Gteq { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a >= b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a >= b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a as u8 >= b as u8),
                        _ => panic!("Invalid types for greater than or equal"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Lteq { addr1, addr2, addr3 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a <= b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a <= b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a as u8 <= b as u8),
                        _ => panic!("Invalid types for less than or equal"),
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Not { addr1, addr2 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let result = match value1 {
                        Value::Bool(a) => Value::Bool(!a),
                        _ => panic!("Invalid type for negation"),
                    };
                    self.memory.set_value(stack_block, *addr2, result);
                    self.next_instr();
                }
                Instructions::Open { function, addr } => {
                    let frame = StackFrame {
                        block: todo!("allocate new block"),
                        return_value: *addr,
                        return_addr: self.instr_ptr + 1,
                        function: *function,
                    };
                    self.stack_frames.next = frame;
                }
                Instructions::Arg { addr, to } => {
                    let value = self.memory.get_value(stack_block, *addr);
                    self.memory.set_value(self.stack_frames.next.block, *to, value);
                    self.next_instr();
                }
                Instructions::Jump => {
                    self.instr_ptr = self.ctx.module.functions[self.stack_frames.next.function].start;
                    stack_block = self.stack_frames.next.block;
                    self.stack_frames.push(self.stack_frames.next.clone());
                }
                Instructions::Return { addr } => {
                    let value = self.memory.get_value(stack_block, *addr);
                    let frame = self.stack_frames.pop().unwrap();
                    let prev_block = self.stack_frames.frames.last().unwrap().block;
                    self.memory.set_value(prev_block, frame.return_value, value);
                    self.instr_ptr = frame.return_addr;
                    stack_block = prev_block;
                }
            }
        }
    }

    pub fn next_instr(&mut self) {
        self.instr_ptr += 1;
    }
}

#[cfg(test)]
mod test {
    use core::panic;

    use super::*;

    #[test]
    fn test_context() {
        let context = Context::default();
        let thread = context.create_thread();
        assert_eq!(thread.instr_ptr, 0);
    }

    #[test]
    fn test_thread() {
        let mut context = Context::default();
        context.instructions = vec![
            Instructions::Load { value: Value::Int(5), addr: 0 },
            Instructions::Load { value: Value::Int(10), addr: 1 },
            Instructions::Add { addr1: 0, addr2: 1, addr3: 2 },
            Instructions::Debug { addr: 2 },
            Instructions::End { exit_value: 2 },
        ];
        context.entry_instruction = 0;
        context.module.functions.push(Function {
            name: "main".to_string(),
            args: vec![],
            ret: Type {
                kind: Types::Word("int".to_string()),
                refs: 0,
                line: Line {
                    line: 0,
                    column: 0,
                    file: "".to_string(),
                },
            },
            start: 0,
            end: 4,
            line: Line {
                line: 0,
                column: 0,
                file: "".to_string(),
            },
        });
        let mut thread = context.create_thread();
        thread.memory.blocks.blocks.push(Block {
            data: vec![Value::Int(0), Value::Int(0), Value::Int(0)],
            free: false,
            count: 0,
        });
        thread.stack_frames.frames.push(StackFrame {
            block: 0,
            return_value: 0,
            return_addr: 0,
            function: 0,
        });
        let value = thread.run();

        assert_eq!(value, Value::Int(15));

        panic!("Just testing the output, everything is fine!")
    }

    #[test]
    /// simple iteration benchmark
    /// 
    /// This test is used to benchmark the runtime
    /// 
    /// # Results
    /// 
    /// On my machine, this test takes about 17.6896ms to run
    /// 
    /// This is a big improvement from my previous language, which took about 63ms to run
    /// 
    /// Also a lot better than Python, which took about 58ms to run (although while loop was used in Python)
    /// 
    /// If Python was used with a for loop, it would take about 20ms to run which is still slower than this runtime
    /// 
    /// # Note
    /// 
    /// Run test with `cargo test --release` to get accurate results or else the test might take up to 10x longer to run
    /// 
    /// This test is not accurate and should not be used as a benchmark
    /// 
    /// This is just a simple test to see how the runtime performs
    fn benchmark() {
        const ITERATIONS: i64 = 1_000_000;

        let mut context = Context::default();
        const N: usize = 0;
        const ITERS: usize = 1;
        const ADD: usize = 2;
        const BRANCH: usize = 3;
        context.instructions = vec![
            // 0
            Instructions::Load { value: Value::Int(0), addr: N },
            Instructions::Load { value: Value::Int(ITERATIONS), addr: ITERS },
            Instructions::Load { value: Value::Int(1), addr: ADD },

            // 3
            Instructions::Eq { addr1: N, addr2: ITERS, addr3: BRANCH },
            Instructions::Not { addr1: BRANCH, addr2: BRANCH },
            Instructions::Branch { cond: BRANCH, addr: 6, else_: 8 },

            // 6
            Instructions::Add { addr1: N, addr2: ADD, addr3: N },
            Instructions::Goto { addr: 3 },

            // 8
            Instructions::End { exit_value: N },
        ];
        context.entry_instruction = 0;
        context.module.functions.push(Function {
            name: "main".to_string(),
            args: vec![],
            ret: Type {
                kind: Types::Word("int".to_string()),
                refs: 0,
                line: Line {
                    line: 0,
                    column: 0,
                    file: "".to_string(),
                },
            },
            start: 0,
            end: 7,
            line: Line {
                line: 0,
                column: 0,
                file: "".to_string(),
            },
        });
        let mut thread = context.create_thread();
        thread.memory.blocks.blocks.push(Block {
            data: vec![Value::Int(0), Value::Int(0), Value::Int(0), Value::Int(0)],
            free: false,
            count: 0,
        });
        thread.stack_frames.frames.push(StackFrame {
            block: 0,
            return_value: 0,
            return_addr: 0,
            function: 0,
        });

        let start = std::time::Instant::now();
        let value = thread.run();
        let elapsed = start.elapsed();
        println!("Elapsed: {:?}", elapsed);

        assert_eq!(value, Value::Int(ITERATIONS));

        panic!("Just testing the output, everything is fine!")
    }
}