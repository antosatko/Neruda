use memory::*;
use module::*;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Hold necessary data for starting the main thread
///
/// After the main thread is created, this struct will be consumed
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Context {
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
                function: 0,
            },
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

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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
    Branch {
        cond: StackAddr,
        addr: InstrAddr,
        else_: InstrAddr,
    },
    /// Additive goto, adds <addr> to the current instruction pointer
    GotoAdd { addr: InstrAddr },
    /// Additive branch, adds <addr> to the current instruction pointer if the value in stack[<cond>] is true else adds <else_> to the current instruction pointer
    BranchAdd {
        cond: StackAddr,
        addr: InstrAddr,
        else_: InstrAddr,
    },
    /// Subractive goto, subtracts <addr> from the current instruction pointer
    GotoSub { addr: InstrAddr },
    /// Subractive branch, subtracts <addr> from the current instruction pointer if the value in stack[<cond>] is true else subtracts <else_> from the current instruction pointer
    BranchSub {
        cond: StackAddr,
        addr: InstrAddr,
        else_: InstrAddr,
    },

    /// Adds the values in stack[<addr1>] and stack[<addr2>] and stores the result in stack[<addr3>]
    Add {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Subtracts the value in stack[<addr2>] from the value in stack[<addr1>] and stores the result in stack[<addr3>]
    Sub {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Multiplies the values in stack[<addr1>] and stack[<addr2>] and stores the result in stack[<addr3>]
    Mul {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Divides the value in stack[<addr1>] by the value in stack[<addr2>] and stores the result in stack[<addr3>]
    Div {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Modulus of the value in stack[<addr1>] by the value in stack[<addr2>] and stores the result in stack[<addr3>]
    Mod {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },

    /// Compares for stack[<addr1>] equal to stack[<addr2>] and stores the result in stack[<addr3>]
    Eq {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Compares for stack[<addr1>] greater than stack[<addr2>] and stores the result in stack[<addr3>]
    Gt {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Compares for stack[<addr1>] less than stack[<addr2>] and stores the result in stack[<addr3>]
    Lt {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Compares for stack[<addr1>] greater than or equal to stack[<addr2>] and stores the result in stack[<addr3>]
    Gteq {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
    /// Compares for stack[<addr1>] less than or equal to stack[<addr2>] and stores the result in stack[<addr3>]
    Lteq {
        addr1: StackAddr,
        addr2: StackAddr,
        addr3: StackAddr,
    },
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

    /// Allocates a new block of memory with the <size> and stores the pointer in stack[<addr>]
    AllocStatic { size: usize, addr: StackAddr },
    /// Allocates a new block of memory with the size in stack[<size>] and stores the pointer in stack[<addr>]
    AllocDynamic { size: StackAddr, addr: StackAddr },
    /// Allocates a new block of memory with the size in module.<kind>[<ID>].size and stores the pointer in stack[<addr>]
    AllocId {
        id: ID,
        kind: ModuleType,
        addr: StackAddr,
    },
    /// Deallocates the block of memory in stack[<addr>]
    Dealloc { addr: StackAddr },
    /// Reallocates the block of memory in stack[<addr>] with the new size in stack[<size>]
    Realloc { addr: StackAddr, size: StackAddr },

    /// GC call to collect garbage | slower | frees memory | slower future allocations
    CollectGarbage,
    /// GC call to mark garbage | faster | does not free memory | faster future allocations
    MarkGarbage,

    /// Reads the value that pointer in stack[<ptr>] points to and stores it in stack[<to>]
    ReadPtr { ptr: StackValue, to: StackAddr },
    /// Writes the value in stack[<value>] to the pointer in stack[<ptr>]
    WritePtr { ptr: StackValue, value: StackValue },
    /// Poiter arithmetic, adds the value in stack[<offset>] to the pointer in stack[<ptr>] and stores the result in stack[<to>]
    PtrAdd {
        ptr: StackValue,
        add_offset: StackValue,
        to: StackAddr,
    },

    /// Calls the native library function with the <id> and stores the return value in stack[<addr>]
    CallNative {
        lib: ID,
        function: ID,
        addr: StackAddr,
    },
}

pub mod module {
    use crate::api::NativeLib;
    use serde::{Deserialize, Serialize};

    /// Module contains definitions for functions, classes, closures, arrays, tuples, and strings
    #[derive(Debug, Clone, Default, Serialize, Deserialize)]
    pub struct Module {
        pub functions: Vec<Function>,
        pub classes: Vec<Class>,
        pub closures: Vec<Closure>,
        pub arrays: Vec<Array>,
        pub tuples: Vec<Tuple>,
        pub strings: Vec<String>,
        #[serde(skip)]
        pub native_libs: Vec<NativeLib>,
    }

    #[derive(Serialize, Deserialize, Debug, Clone, Copy)]
    #[repr(C)]
    pub enum ModuleType {
        Function,
        Class,
        Closure,
        Array,
        Tuple,
        String,
    }

    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub struct Line {
        pub line: usize,
        pub column: usize,
        pub file: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
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

    #[derive(Serialize, Deserialize, Debug, Clone)]
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
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Function {
        pub name: String,
        pub stack_size: usize,
        pub args: Vec<(String, Type)>,
        pub ret: Type,
        /// The index of the first instruction in the function
        pub start: usize,
        /// The index of the last instruction in the function
        pub end: usize,
        pub line: Line,
    }

    /// Class definition
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Class {
        pub name: String,
        pub fields: Vec<(String, Type)>,
        pub methods: Vec<Function>,
        pub line: Line,
    }

    /// Closure definition
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Closure {
        pub name: String,
        pub args: Vec<(String, Type)>,
        pub start: usize,
        pub end: usize,
        pub line: Line,
    }

    /// Array definition
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Array {
        pub type_: Type,
        pub line: Line,
    }

    /// Tuple definition
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Tuple {
        pub types: Vec<Type>,
        pub line: Line,
    }
}

pub mod memory {
    use serde::{Deserialize, Serialize};

    use crate::StackFrames;

    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
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

        Closure { instr_ptr: usize, block: usize },
        Function { instr_ptr: usize },

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

    impl Allocator<Block> for Blocks {
        fn allocate(&mut self, obj: Block) -> usize {
            match self.free.pop() {
                Some((index, _)) => {
                    self.blocks[index] = obj;
                    index
                }
                None => {
                    self.blocks.push(obj);
                    self.blocks.len() - 1
                }
            }
        }

        fn deallocate(&mut self, index: usize) {
            let block = &mut self.blocks[index];
            if block.protect {
                return;
            }
            block.free = true;
            self.free.push((index, block.data.len()));
        }

        fn realloc(&mut self, index: usize, size: usize) {
            self.blocks[index].data.resize(size, Value::Null);
        }
    }

    impl Allocator<StringObject> for Strings {
        fn allocate(&mut self, obj: StringObject) -> usize {
            match self.free.pop() {
                Some(index) => {
                    self.data[index] = obj;
                    index
                }
                None => {
                    self.data.push(obj);
                    self.data.len() - 1
                }
            }
        }

        fn deallocate(&mut self, index: usize) {
            self.data[index].free = true;
            self.free.push(index);
        }

        fn realloc(&mut self, _index: usize, _size: usize) {
            return;
        }
    }

    impl Allocator<UDContainer> for UDHeap {
        fn allocate(&mut self, obj: UDContainer) -> usize {
            match self.free.pop() {
                Some(index) => {
                    self.data[index] = obj;
                    index
                }
                None => {
                    self.data.push(obj);
                    self.data.len() - 1
                }
            }
        }

        fn deallocate(&mut self, index: usize) {
            let ud = &mut self.data[index];
            ud.free = true;
            self.free.push(index);
            ud.data.collect();
        }

        fn realloc(&mut self, _index: usize, _size: usize) {
            return;
        }
    }

    /// A block of memory
    #[derive(Debug, Clone)]
    pub struct Block {
        /// Data stored in the block
        pub data: Vec<Value>,
        /// Block that is free to use
        pub free: bool,
        /// Protected blocks can not be deallocated manually
        ///
        /// Only the garbage collector can deallocate protected blocks
        ///
        /// This is useful for blocks that are captured by closures
        pub protect: bool,
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
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
    pub enum GCMethod {
        /// The data is not garbage collected
        None,
        /// The data is garbage collected
        GC,
        /// The data is garbage collected and the garbage collector will call the `collect` method
        Prepare,
    }

    impl Memory {
        #[inline]
        pub fn get_value(&self, stack_block: usize, addr: usize) -> Value {
            self.blocks.blocks[stack_block].data[addr]
        }

        #[inline]
        pub fn set_value(&mut self, stack_block: usize, addr: usize, value: Value) {
            self.blocks.blocks[stack_block].data[addr] = value;
        }

        #[inline]
        pub fn get_string(&self, addr: usize) -> String {
            self.strings.data[addr].data.clone()
        }

        #[inline]
        pub fn set_string(&mut self, addr: usize, value: String) {
            self.strings.data[addr].data = value;
        }

        fn mark_all(&mut self) {
            for block in &mut self.blocks.blocks {
                block.free = true;
            }
            for string in &mut self.strings.data {
                string.free = true;
            }
            for ud in &mut self.userdata.data {
                ud.free = true;
            }
        }

        fn mark_used_block(&mut self, block: usize) {
            self.blocks.blocks[block].free = false;

            let mut i = 0;
            while let Some(value) = self.blocks.blocks[block].data.get(i) {
                match value {
                    Value::Block { block } => {
                        self.mark_used_block(*block);
                    }
                    Value::Pointer { block, .. } => {
                        self.mark_used_block(*block);
                    }
                    Value::String { str } => {
                        self.strings.data[*str].free = false;
                    }
                    Value::CharPtr { str, .. } => {
                        self.strings.data[*str].free = false;
                    }
                    Value::Userdata { data } => {
                        self.userdata.data[*data].free = false;
                    }
                    Value::Closure { block, .. } => {
                        self.mark_used_block(*block);
                    }
                    _ => {}
                }
                i += 1;
            }
        }

        fn mark(&mut self, stack_frames: &StackFrames) {
            self.mark_all();

            for frame in &stack_frames.frames {
                self.mark_used_block(frame.block);
            }
        }

        fn sweep(&mut self) {
            self.blocks.free.clear();
            self.blocks.free.reserve(self.blocks.blocks.len());
            self.strings.free.clear();
            self.strings.free.reserve(self.strings.data.len());
            self.userdata.free.clear();
            self.userdata.free.reserve(self.userdata.data.len());

            for (index, block) in self.blocks.blocks.iter_mut().enumerate() {
                if block.free {
                    self.blocks.free.push((index, block.data.len()));
                }
            }
            for (index, string) in self.strings.data.iter_mut().enumerate() {
                if string.free {
                    self.strings.free.push(index);
                }
            }
            for (index, ud) in self.userdata.data.iter_mut().enumerate() {
                if ud.free {
                    match ud.data.gc_method() {
                        GCMethod::GC => {
                            self.userdata.free.push(index);
                        }
                        GCMethod::Prepare => {
                            ud.data.collect();
                            self.userdata.free.push(index);
                        }
                        GCMethod::None => {}
                    }
                }
            }
        }

        fn shrink(&mut self) {
            let mut max = 0;
            for (index, block) in self.blocks.blocks.iter().enumerate().rev() {
                if !block.free {
                    max = index;
                    break;
                }
            }
            self.blocks.blocks.truncate(max + 1);
            let mut max = 0;
            for (index, string) in self.strings.data.iter().enumerate().rev() {
                if !string.free {
                    max = index;
                    break;
                }
            }
            self.strings.data.truncate(max + 1);
            let mut max = 0;
            for (index, ud) in self.userdata.data.iter().enumerate().rev() {
                if !ud.free {
                    max = index;
                    break;
                }
            }
            self.userdata.data.truncate(max + 1);

            self.sweep()
        }

        /// Collect garbage
        ///
        /// Free all excess memory
        ///
        /// This is more expensive than `mark_garbage`, use this only when you need to free memory
        /// or when performance is not an issue
        ///
        /// Freeing memory also means that future allocations might run out of preallocated memory
        /// and will need to allocate more memory resulting in slighly slower performance
        pub fn collect_garbage(&mut self, stack_frames: &StackFrames) {
            self.mark(stack_frames);
            self.sweep();
            self.shrink();
        }

        /// Mark garbage
        ///
        /// This will only mark the garbage, it will not free the memory
        /// but the memory will be available for reuse
        ///
        /// This is faster than `collect_garbage`, use this when performance is an issue
        pub fn mark_garbage(&mut self, stack_frames: &StackFrames) {
            self.mark(stack_frames);
            self.sweep();
        }
    }

    impl Block {
        pub fn new(size: usize) -> Self {
            Self {
                data: vec![Value::Null; size],
                free: false,
                protect: false,
            }
        }

        pub fn protected(size: usize) -> Self {
            Self {
                data: vec![Value::Null; size],
                free: false,
                protect: true,
            }
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
    /// Create a new thread that has the same context as the current thread
    pub fn copy_new(&self) -> Self {
        Self {
            memory: Memory::default(),
            instr_ptr: 0,
            stack_frames: StackFrames::new(),
            ctx: self.ctx.clone(),
        }
    }

    /// Run the thread
    ///
    /// Entry point is ID of the function in the module that the thread will start from
    pub fn run(&mut self, entry: usize) -> Result<Value, Error> {
        let mut stack_block = match self.stack_frames.frames.last() {
            Some(frame) => frame.block,
            None => self
                .memory
                .blocks
                .allocate(Block::new(self.ctx.module.functions[entry].stack_size)),
        };
        self.instr_ptr = self.ctx.module.functions[entry].start;
        self.stack_frames.push(StackFrame {
            block: stack_block,
            return_value: 0,
            return_addr: 0,
            function: entry,
        });
        loop {
            let instr = &self.ctx.instructions[self.instr_ptr];
            match instr {
                Instructions::GotoAdd { addr } => {
                    self.instr_ptr += addr;
                }
                Instructions::BranchAdd { cond, addr, else_ } => {
                    let value = self.memory.get_value(stack_block, *cond);
                    if let Value::Bool(true) = value {
                        self.instr_ptr += addr;
                    } else {
                        self.instr_ptr += else_;
                    }
                }
                Instructions::GotoSub { addr } => {
                    self.instr_ptr -= addr;
                }
                Instructions::BranchSub { cond, addr, else_ } => {
                    let value = self.memory.get_value(stack_block, *cond);
                    if let Value::Bool(true) = value {
                        self.instr_ptr -= addr;
                    } else {
                        self.instr_ptr -= else_;
                    }
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
                Instructions::Load { value, addr } => {
                    self.memory.set_value(stack_block, *addr, *value);
                    self.next_instr();
                }
                Instructions::Add {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a + b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                        (Value::Char(a), Value::Char(b)) => {
                            Value::Char((a as u8 + b as u8) as char)
                        }
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Sub {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a - b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                        (Value::Char(a), Value::Char(b)) => {
                            Value::Char((a as u8 - b as u8) as char)
                        }
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Mul {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a * b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                        (Value::Char(a), Value::Char(b)) => {
                            Value::Char((a as u8 * b as u8) as char)
                        }
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Div {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a / b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                        (Value::Char(a), Value::Char(b)) => {
                            Value::Char((a as u8 / b as u8) as char)
                        }
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Mod {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Uint(a % b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
                        (Value::Char(a), Value::Char(b)) => {
                            Value::Char((a as u8 % b as u8) as char)
                        }
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Eq {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a == b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a == b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a == b),
                        (Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Gt {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a > b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a > b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a as u8 > b as u8),
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Lt {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a < b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a < b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool((a as u8) < b as u8),
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Gteq {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a >= b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a >= b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a as u8 >= b as u8),
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Lteq {
                    addr1,
                    addr2,
                    addr3,
                } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let value2 = self.memory.get_value(stack_block, *addr2);
                    let result = match (value1, value2) {
                        (Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
                        (Value::Uint(a), Value::Uint(b)) => Value::Bool(a <= b),
                        (Value::Float(a), Value::Float(b)) => Value::Bool(a <= b),
                        (Value::Char(a), Value::Char(b)) => Value::Bool(a as u8 <= b as u8),
                        _ => {
                            return Err(Error::InvalidTypes {
                                instr: self.instr_ptr,
                                value1,
                                value2,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr3, result);
                    self.next_instr();
                }
                Instructions::Not { addr1, addr2 } => {
                    let value1 = self.memory.get_value(stack_block, *addr1);
                    let result = match value1 {
                        Value::Bool(a) => Value::Bool(!a),
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *addr2, result);
                    self.next_instr();
                }
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
                Instructions::Open { function, addr } => {
                    let frame = StackFrame {
                        block: self
                            .memory
                            .blocks
                            .allocate(Block::new(self.ctx.module.functions[*function].stack_size)),
                        return_value: *addr,
                        return_addr: self.instr_ptr + 1,
                        function: *function,
                    };
                    self.stack_frames.next = frame;
                    self.next_instr();
                }
                Instructions::Arg { addr, to } => {
                    let value = self.memory.get_value(stack_block, *addr);
                    self.memory
                        .set_value(self.stack_frames.next.block, *to, value);
                    self.next_instr();
                }
                Instructions::Jump => {
                    self.stack_frames.next.return_addr = self.instr_ptr + 1;
                    self.instr_ptr =
                        self.ctx.module.functions[self.stack_frames.next.function].start;
                    stack_block = self.stack_frames.next.block;
                    self.stack_frames.push(self.stack_frames.next.clone());
                }
                Instructions::Return { addr } => {
                    self.memory.blocks.deallocate(stack_block);
                    let value = self.memory.get_value(stack_block, *addr);
                    let current_frame = match self.stack_frames.pop() {
                        Some(frame) => frame,
                        None => return Err(Error::LostCurrentFrame),
                    };
                    let prev_block = match self.stack_frames.frames.last() {
                        Some(frame) => frame.block,
                        None => return Ok(value),
                    };
                    self.memory
                        .set_value(prev_block, current_frame.return_value, value);
                    self.instr_ptr = current_frame.return_addr;
                    stack_block = prev_block;
                }
                Instructions::AllocStatic { size, addr } => {
                    let block = Block::new(*size);
                    let block_index = self.memory.blocks.allocate(block);
                    self.memory
                        .set_value(stack_block, *addr, Value::Block { block: block_index });
                    self.next_instr();
                }
                Instructions::AllocDynamic { size, addr } => {
                    let size = self.memory.get_value(stack_block, *size);
                    let block = Block::new(match size {
                        Value::Int(size) => size as usize,
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: size,
                            })
                        }
                    });
                    let block_index = self.memory.blocks.allocate(block);
                    self.memory
                        .set_value(stack_block, *addr, Value::Block { block: block_index });
                    self.next_instr();
                }
                Instructions::AllocId { id, kind, addr } => {
                    let size = match kind {
                        ModuleType::Array => 1,
                        ModuleType::Tuple => self.ctx.module.tuples[*id].types.len(),
                        ModuleType::Class => self.ctx.module.classes[*id].fields.len(),
                        _ => {
                            return Err(Error::InvalidModuleType {
                                instr: self.instr_ptr,
                                kind: *kind,
                            })
                        }
                    };
                    let block = Block::new(size);
                    let block_index = self.memory.blocks.allocate(block);
                    self.memory
                        .set_value(stack_block, *addr, Value::Block { block: block_index });
                    self.next_instr();
                }
                Instructions::Dealloc { addr } => {
                    match self.memory.get_value(stack_block, *addr) {
                        Value::Block { block } => self.memory.blocks.deallocate(block),
                        Value::String { str } => self.memory.strings.deallocate(str),
                        Value::Userdata { data } => self.memory.userdata.deallocate(data),
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: self.memory.get_value(stack_block, *addr),
                            })
                        }
                    };
                    self.next_instr();
                }
                Instructions::Realloc { addr, size } => {
                    let block = match self.memory.get_value(stack_block, *addr) {
                        Value::Block { block } => block,
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: self.memory.get_value(stack_block, *addr),
                            })
                        }
                    };
                    let size = match self.memory.get_value(stack_block, *size) {
                        Value::Int(size) => size as usize,
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: self.memory.get_value(stack_block, *size),
                            })
                        }
                    };
                    self.memory.blocks.realloc(block, size);
                    self.next_instr();
                }
                Instructions::ReadPtr { ptr, to } => {
                    let ptr = self.memory.get_value(stack_block, *ptr);
                    let value = match ptr {
                        Value::Pointer { block, offset } => self.memory.get_value(block, offset),
                        Value::CharPtr { str, offset } => {
                            let string = self.memory.get_string(str);
                            Value::Char(
                                string
                                    .chars()
                                    .nth(offset)
                                    .expect("TODO: decide what to do when out of bounds"),
                            )
                        }
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: ptr,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *to, value);
                    self.next_instr();
                }
                Instructions::WritePtr { ptr, value } => {
                    let value = self.memory.get_value(stack_block, *value);
                    let ptr = self.memory.get_value(stack_block, *ptr);
                    match ptr {
                        Value::Pointer { block, offset } => {
                            self.memory.set_value(block, offset, value);
                        }
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: ptr,
                            })
                        }
                    }
                    self.next_instr();
                }
                Instructions::PtrAdd {
                    ptr,
                    add_offset,
                    to,
                } => {
                    let ptr = self.memory.get_value(stack_block, *ptr);
                    let add_offset = match self.memory.get_value(stack_block, *add_offset) {
                        Value::Int(offset) => offset as usize,
                        Value::Uint(offset) => offset as usize,
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: self.memory.get_value(stack_block, *add_offset),
                            })
                        }
                    };
                    let new_ptr = match ptr {
                        Value::Pointer { block, offset } => {
                            let new_offset = offset + add_offset;
                            Value::Pointer {
                                block,
                                offset: new_offset,
                            }
                        }
                        Value::CharPtr { str, offset } => {
                            let new_offset = offset + add_offset;
                            Value::CharPtr {
                                str,
                                offset: new_offset,
                            }
                        }
                        Value::String { str } => {
                            let new_offset = add_offset;
                            Value::CharPtr {
                                str,
                                offset: new_offset,
                            }
                        }
                        Value::Block { block } => {
                            let new_offset = add_offset;
                            Value::Pointer {
                                block,
                                offset: new_offset,
                            }
                        }
                        _ => {
                            return Err(Error::InvalidType {
                                instr: self.instr_ptr,
                                value1: ptr,
                            })
                        }
                    };
                    self.memory.set_value(stack_block, *to, new_ptr);
                    self.next_instr();
                }
                Instructions::LoadString { str, addr } => {
                    let str = self.memory.strings.allocate(StringObject {
                        data: self.ctx.module.strings[*str].clone(),
                        free: false,
                    });
                    self.memory.set_value(stack_block, *addr, Value::String { str });
                    self.next_instr();
                }
                Instructions::CallNative {
                    lib,
                    function,
                    addr,
                } => {
                    let lib = self.ctx.module.native_libs[*lib];
                    let addr = *addr;
                    let value = match lib(self, *function) {
                        Ok(value) => value,
                        Err(err) => return Err(Error::NativeLibErr(err)),
                    };
                    self.memory.set_value(stack_block, addr, value);
                    self.next_instr();
                }
                Instructions::Debug { addr } => {
                    let value = self.memory.get_value(stack_block, *addr);
                    println!("{:?}", value);
                    self.next_instr();
                }
                Instructions::CollectGarbage => {
                    self.memory.collect_garbage(&self.stack_frames);
                    self.next_instr();
                }
                Instructions::MarkGarbage => {
                    self.memory.mark_garbage(&self.stack_frames);
                    self.next_instr();
                }
                Instructions::End { exit_value } => {
                    let value = self.memory.get_value(stack_block, *exit_value);
                    return Ok(value);
                }
                Instructions::Noop => self.next_instr(),
            }
        }
    }

    #[inline]
    pub fn next_instr(&mut self) {
        self.instr_ptr += 1;
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidTypes {
        instr: usize,
        value1: Value,
        value2: Value,
    },
    InvalidType {
        instr: usize,
        value1: Value,
    },
    InvalidModuleType {
        instr: usize,
        kind: ModuleType,
    },
    NativeLibErr(api::NativeLibErr),
    LostCurrentFrame,
}

#[cfg(test)]
mod test {
    use core::panic;
    use std::io::Write;

    use crate::api::NativeLibErr;

    use super::*;

    #[test]
    fn test_context() {
        let context = Context::default();
        let thread = context.create_thread();
        assert_eq!(thread.instr_ptr, 0);
    }

    #[test]
    fn test_thread() -> anyhow::Result<()> {
        let mut context = Context::default();
        context.instructions = vec![
            Instructions::Load {
                value: Value::Int(5),
                addr: 0,
            },
            Instructions::Load {
                value: Value::Int(10),
                addr: 1,
            },
            Instructions::Add {
                addr1: 0,
                addr2: 1,
                addr3: 2,
            },
            Instructions::Debug { addr: 2 },
            Instructions::End { exit_value: 2 },
        ];
        context.module.functions.push(Function {
            name: "main".to_string(),
            stack_size: 3,
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
        let value = thread.run(0).unwrap();

        assert_eq!(value, Value::Int(15));

        if FAIL_ALL {
            panic!("Just testing the output, everything is fine!")
        }
        Ok(())
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
    ///
    /// # Note 2
    ///
    /// After inlining `next_instr`, `set_value`, and `get_value` functions, the runtime took about 14ms to run (nice)
    fn benchmark() -> anyhow::Result<()> {
        const ITERATIONS: i64 = 1_000_000 - 999999;

        let mut context = Context::default();
        const N: usize = 0;
        const ITERS: usize = 1;
        const ADD: usize = 2;
        const BRANCH: usize = 3;
        context.instructions = vec![
            // 0
            Instructions::Load {
                value: Value::Int(0),
                addr: N,
            },
            Instructions::Load {
                value: Value::Int(ITERATIONS),
                addr: ITERS,
            },
            Instructions::Load {
                value: Value::Int(1),
                addr: ADD,
            },
            // 3
            Instructions::Eq {
                addr1: N,
                addr2: ITERS,
                addr3: BRANCH,
            },
            Instructions::Not {
                addr1: BRANCH,
                addr2: BRANCH,
            },
            Instructions::Noop, // MarkGarbage 24ms | CollectGarbage 30ms // this was added to test the performance of the garbage collector
            Instructions::Branch {
                cond: BRANCH,
                addr: 7,
                else_: 9,
            },
            // 7
            Instructions::Add {
                addr1: N,
                addr2: ADD,
                addr3: N,
            },
            Instructions::Goto { addr: 3 },
            // 9
            Instructions::End { exit_value: N },
        ];
        context.module.functions.push(Function {
            name: "main".to_string(),
            stack_size: 4,
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
            end: 9,
            line: Line {
                line: 0,
                column: 0,
                file: "".to_string(),
            },
        });

        let serialized = bincode::serialize(&context).unwrap();
        let file = std::fs::File::create("../junkfiles/test.bin").unwrap();
        let mut writer = std::io::BufWriter::new(file);
        writer.write_all(&serialized).unwrap();

        let serialized = serde_json::to_string(&context).unwrap();
        let file = std::fs::File::create("../junkfiles/test.json").unwrap();
        let mut writer = std::io::BufWriter::new(file);
        writer.write_all(serialized.as_bytes()).unwrap();

        let mut thread = context.create_thread();

        let start = std::time::Instant::now();
        let value = thread.run(0).unwrap();
        let elapsed = start.elapsed();
        println!("Elapsed: {:?}", elapsed);

        assert_eq!(value, Value::Int(ITERATIONS));

        if FAIL_ALL {
            panic!("Just testing the output, everything is fine!")
        }

        Ok(())
    }

    #[test]
    fn native() -> anyhow::Result<()> {
        fn lib(th: &mut Thread, id: usize) -> Result<Value, NativeLibErr> {
            match id {
                0 => Ok(Value::Int(5)),
                _ => Err(NativeLibErr::NotFound),
            }
        }

        let mut context = Context::default();
        context.module.native_libs.push(lib);
        context.instructions = vec![
            Instructions::CallNative {
                lib: 0,
                function: 0,
                addr: 0,
            },
            Instructions::Debug { addr: 0 },
            Instructions::Return { addr: 0 },
        ];
        context.module.functions.push(Function {
            name: "main".to_string(),
            stack_size: 1,
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
            end: 2,
            line: Line {
                line: 0,
                column: 0,
                file: "".to_string(),
            },
        });

        let mut thread = context.create_thread();
        let value = thread.run(0).unwrap();

        assert_eq!(value, Value::Int(5));

        if FAIL_ALL {
            panic!("Just testing the output, everything is fine!")
        }

        Ok(())
    }

    #[test]
    fn fib() {
        let start_mem = PEAK_ALLOC.current_usage_as_kb();
        println!("Memory: {}kB", start_mem);
        let mut context = Context::default();
        context.instructions = vec![
            // main
            // var 0 = 10
            Instructions::Load {
                value: Value::Int(33),
                addr: 0,
            },
            // open fib
            Instructions::Open {
                function: 1,
                addr: 1,
            },
            // arg 0 = 10; var 1 = fib(10)
            Instructions::Arg {
                addr: 0,
                to: 0,
            },
            // call fib
            Instructions::Jump,
            Instructions::End { exit_value: 1 },

            // fib; var 0 = n
            // var 1 = 1
            // if n <= 1 return n
            Instructions::Load {
                value: Value::Int(1),
                addr: 1,
            },
            // var 3 = n <= 1
            Instructions::Lteq {
                addr1: 0,
                addr2: 1,
                addr3: 2,
            },
            // if var 3 return n
            Instructions::BranchAdd {
                cond: 2,
                addr: 1,
                else_: 2,
            },
            // return n
            Instructions::Return { addr: 0 },

            // var 2 = n - 1
            Instructions::Sub {
                addr1: 0,
                addr2: 1,
                addr3: 2,
            },
            // var 4 = fib(n - 1)
            Instructions::Open {
                function: 1,
                addr: 4,
            },
            // arg 0 = n - 1
            Instructions::Arg {
                addr: 2,
                to: 0,
            },
            // call fib
            Instructions::Jump,
            // var 5 = n - 2
            Instructions::Sub {
                addr1: 2, // previous result (n - 1)
                addr2: 1,
                addr3: 5,
            },
            // var 6 = fib(n - 2)
            Instructions::Open {
                function: 1,
                addr: 6,
            },
            // arg 0 = n - 2
            Instructions::Arg {
                addr: 5,
                to: 0,
            },
            // call fib
            Instructions::Jump,
            // var 7 = var 4 + var 6
            Instructions::Add {
                addr1: 4,
                addr2: 6,
                addr3: 7,
            },
            // return var 7
            Instructions::Return { addr: 7 },
        ];
        context.module.functions.push(Function {
            name: "main".to_string(),
            stack_size: 2,
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
            end: 3,
            line: Line {
                line: 0,
                column: 0,
                file: "".to_string(),
            },
        });
        context.module.functions.push(Function {
            name: "fib".to_string(),
            stack_size: 8,
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
            start: 5,
            end: 17,
            line: Line {
                line: 0,
                column: 0,
                file: "".to_string(),
            },
        });
        let ctx_mem = PEAK_ALLOC.current_usage_as_kb();
        println!("Memory: {}kB", ctx_mem);

        let serialized = bincode::serialize(&context).unwrap();
        let file = std::fs::File::create("../junkfiles/fib.bin").unwrap();
        let mut writer = std::io::BufWriter::new(file);
        writer.write_all(&serialized).unwrap();

        let serialized = serde_json::to_string(&context).unwrap();
        let file = std::fs::File::create("../junkfiles/fib.json").unwrap();
        let mut writer = std::io::BufWriter::new(file);
        writer.write_all(serialized.as_bytes()).unwrap();

        let mut thread = context.create_thread();
        let thread_mem = PEAK_ALLOC.current_usage_as_kb();
        println!("Memory: {}kB", thread_mem);


        let time = std::time::Instant::now();
        let value = thread.run(0).unwrap();
        println!("Elapsed: {:?}", time.elapsed());

        let peak_mem = PEAK_ALLOC.peak_usage_as_kb();

        println!("{:?}", value);
        println!("Memory: {}kB -> {}kB -> {}kB", start_mem, ctx_mem, thread_mem);
        println!("Peak Memory: {}kB", peak_mem);

        assert_eq!(value, Value::Int(55));

        if FAIL_ALL {
            panic!("Just testing the output, everything is fine!")
        }
    }

    /// If true, all tests will fail so that the output can be seen
    pub const FAIL_ALL: bool = false;

    use peak_alloc::PeakAlloc;
    #[global_allocator]
    static PEAK_ALLOC: PeakAlloc = PeakAlloc;
}

