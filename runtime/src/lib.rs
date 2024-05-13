use serde::{Deserialize, Serialize};

/// Context for Neruda VM
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Context {
    pub memory: Memory,
    pub executor: Executor,
    pub module: Module,
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
                heap: Heap {
                    objects: vec![],
                    marked: vec![],
                    freed: vec![],
                },
                strings: Strings {
                    data: vec![],
                    marked: vec![],
                    freed: vec![],
                },
            },
            executor: Executor {
                instructions: vec![],
                instruction_pointer: 0,
            },
            module: Module {
                functions: vec![],
                classes: vec![],
                closures: vec![],
                arrays: vec![],
                tuples: vec![],
            },
        }
    }

    pub fn run(&mut self) -> ProgramReturn {
        loop {
            let instr = self.executor.instructions[self.executor.instruction_pointer as usize];
            match instr {
                Instructions::Debug(stack_pos) => {
                    let value = self.memory.stack.get_value(stack_pos);
                    println!(
                        "DEBUG<instr: {} | val:{}>",
                        self.executor.instruction_pointer, value
                    );
                    self.next_instr();
                }
                Instructions::End => {
                    return ProgramReturn::Ok;
                }
                Instructions::WriteConst(value, stack_pos) => {
                    self.memory.stack.set_value(stack_pos, value);
                    self.next_instr();
                }
                Instructions::Swap(stack_pos1, stack_pos2) => {
                    let (value1, value2) = self.memory.stack.get_2_values(stack_pos1, stack_pos2);
                    self.memory
                        .stack
                        .set_2_values(stack_pos1, stack_pos2, value2, value1);
                    self.next_instr();
                }
                Instructions::Move(stack_pos1, stack_pos2) => {
                    let value = self.memory.stack.get_value(stack_pos1);
                    self.memory.stack.set_value(stack_pos2, value);
                    self.next_instr();
                }
                Instructions::ReadPtr(ptr, stack_pos) => {
                    let ptr = self.memory.stack.get_value(ptr);
                    let value = match self.memory.read_ptr(ptr) {
                        Ok(v) => v,
                        Err(e) => {
                            return ProgramReturn::Error(e);
                        }
                    };
                    self.memory.stack.set_value(stack_pos, value);
                    self.next_instr();
                }
                Instructions::WritePtr(ptr, value) => {
                    let (ptr, value) = self.memory.stack.get_2_values(ptr, value);
                    match self.memory.write_ptr(ptr, value) {
                        Ok(_) => self.next_instr(),
                        Err(e) => {
                            return ProgramReturn::Error(e);
                        }
                    }
                    self.next_instr();
                }
                Instructions::Add(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l + r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 + r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Sub(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l - r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 - r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Mul(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l * r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 * r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Div(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Mod(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l % r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l % r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l % r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 % r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Eq(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Neq(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Gt(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Gte(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Lt(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Lte(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::And(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Or(left, right, pos) => {
                    let (left, right) = self.memory.stack.get_2_values(left, right);
                    let value = match (left, right) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                        (Value::Char(l), Value::Char(r)) => {
                            Value::Char((l as u8 / r as u8) as char)
                        }
                        (a, b) => {
                            return ProgramReturn::Error(Errors::ExpectedGot(a, b));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::Not(val, pos) => {
                    let value = self.memory.stack.get_value(val);
                    let value = match value {
                        Value::Bool(v) => Value::Bool(!v),
                        _ => {
                            return ProgramReturn::Error(Errors::ExpectedGot(
                                Value::Bool(false),
                                value,
                            ));
                        }
                    };
                    self.memory.stack.set_value(pos, value);
                    self.next_instr();
                }
                Instructions::AllocStatic(size, pos) => {
                    let obj = Object::new(ObjectKind {
                        kind: ObjectKinds::Unknown,
                        id: 0,
                    }, size);
                    let id = self.memory.heap.allocate(obj);
                    self.memory.stack.set_value(pos, Value::ObjectPtr(id));
                    self.next_instr();
                }
                Instructions::AllocDynamic(value, pos) => {
                    let value = self.memory.stack.get_value(value);
                    let size = match value {
                        Value::Int(v) => v as u64,
                        Value::Uint(v) => v,
                        Value::Char(v) => v as u64,
                        _ => {
                            return ProgramReturn::Error(Errors::ExpectedGot(Value::Uint(0), value));
                        }
                    };
                    let obj = Object::new(ObjectKind {
                        kind: ObjectKinds::Unknown,
                        id: 0,
                    }, size);
                    let id = self.memory.heap.allocate(obj);
                    self.memory.stack.set_value(pos, Value::ObjectPtr(id));
                    self.next_instr();
                }
                Instructions::AllocKnown(kind, pos) => {
                    let size = self.module.get_size(&kind);
                    let obj = Object::new(kind, size);
                    let id = self.memory.heap.allocate(obj);
                    self.memory.stack.set_value(pos, Value::ObjectPtr(id));
                    self.next_instr();
                }
                Instructions::Dealloc(ptr) => {
                    let ptr = self.memory.stack.get_value(ptr);
                    match ptr {
                        Value::ObjectPtr(id) => {
                            self.memory.heap.deallocate(id);
                        }
                        Value::StringPtr(id) => {
                            self.memory.strings.deallocate(id);
                        }
                        Value::UserdataPtr(_) => todo!(),
                        _ => {
                            return ProgramReturn::Error(Errors::InvalidPointerType(ptr));
                        }
                    }
                    self.next_instr();
                }
                Instructions::Realloc(ptr, size) => {
                    let (ptr, size) = self.memory.stack.get_2_values(ptr, size);
                    let size = match size {
                        Value::Int(v) => v as u64,
                        Value::Uint(v) => v,
                        Value::Char(v) => v as u64,
                        _ => {
                            return ProgramReturn::Error(Errors::ExpectedGot(Value::Uint(0), size));
                        }
                    };
                    match ptr {
                        Value::ObjectPtr(id) => {
                            self.memory.heap.realloc(id, size);
                        }
                        Value::StringPtr(id) => {
                            self.memory.strings.realloc(id, size);
                        }
                        Value::UserdataPtr(_) => todo!(),
                        _ => {
                            return ProgramReturn::Error(Errors::InvalidPointerType(ptr));
                        }
                    }
                    self.next_instr();
                }
                Instructions::Reserve(function_id) => {
                    let fun = &self.module.functions[function_id as usize];
                    let end = self.memory.stack.data.len() as u64 + fun.stack_size;
                    let frame = StackFrame {
                        start: self.memory.stack.data.len() as u64,
                        end,
                        function_id,
                    };
                    if self.memory.stack.frames.len() <= self.memory.stack.frame_pointer as usize {
                        self.memory.stack.frames.push(frame);
                    } else {
                        self.memory.stack.frames[self.memory.stack.frame_pointer as usize] = frame;
                    }
                    self.memory.stack.frame_pointer += 1;
                    self.next_instr();
                }
                Instructions::Release => {
                    self.memory.stack.frame_pointer -= 1;
                    self.next_instr();
                }
                Instructions::Jump(fun_id) => {
                    let fun = &self.module.functions[fun_id as usize];
                    self.executor.instruction_pointer = fun.position;
                }
                Instructions::Return => {
                    self.executor.instruction_pointer =
                        self.memory.stack.frames[self.memory.stack.frame_pointer as usize].end;
                    self.next_instr();
                }
                Instructions::ReturnVal(val, offset) => {
                    let frame =
                        &self.memory.stack.frames[self.memory.stack.frame_pointer as usize - 1];
                    let pos = frame.end - offset as u64;
                    self.memory.stack.data[pos as usize] = self.memory.stack.get_value(val);
                    self.next_instr();
                }
                Instructions::Procedure(_, _) => todo!(),
                Instructions::Goto(instr_addr) => {
                    self.executor.instruction_pointer = instr_addr;
                }
                Instructions::Branch(value, addr1, addr2) => {
                    let value = self.memory.stack.get_value(value);
                    let addr = if let Value::Bool(v) = value {
                        if v {
                            addr1
                        } else {
                            addr2
                        }
                    } else {
                        return ProgramReturn::Error(Errors::ExpectedGot(
                            Value::Bool(false),
                            value,
                        ));
                    };
                    self.executor.instruction_pointer = addr;
                }
                Instructions::Abort(msg) => {
                    return ProgramReturn::Abort(msg);
                }
                Instructions::Index(ptr, val, loc) => {
                    let (ptr, val) = self.memory.stack.get_2_values(ptr, val);
                    let val = match val {
                        Value::Int(v) => v as u64,
                        Value::Uint(v) => v,
                        Value::Char(v) => v as u64,
                        _ => {
                            return ProgramReturn::Error(Errors::ExpectedGot(Value::Uint(0), val));
                        }
                    };
                    let new = match ptr {
                        Value::ObjectPtr(id) => Value::HeapPtr {
                            obj: id,
                            offset: val,
                        },
                        Value::HeapPtr { obj, offset } => Value::HeapPtr {
                            obj,
                            offset: offset + val,
                        },
                        Value::StackPtr(pos) => Value::StackPtr(pos + val),
                        Value::StringPtr(id) => Value::CharPtr {
                            obj: id,
                            offset: val,
                        },
                        Value::CharPtr { obj, offset } => Value::CharPtr {
                            obj,
                            offset: offset + val,
                        },
                        Value::UserdataPtr(_) => todo!(),
                        _ => {
                            return ProgramReturn::Error(Errors::InvalidPointerType(ptr));
                        }
                    };
                    self.memory.stack.set_value(loc, new);
                    self.next_instr();
                }
            }
        }
    }

    pub fn next_instr(&mut self) {
        self.executor.instruction_pointer += 1;
    }
}

/// # Memory layout for Neruda VM
///
/// ## Stack
///
/// The stack is used to store values within functions.
///
/// The stack is divided into stack frames. Each stack frame
/// is used to store values for a single function.
///
/// The stack is divided into two parts:
///
/// - Data: Used to store values
/// - Frames: Used to store stack frame information
///
/// ### Stack Frame
///
/// Each stack frame is divided into multiple parts:
///
/// 1. Arguments: Used to store function when it is called
/// 2. Local variables: Used to store values within the function
/// 3. Return address: Used to store the address to return to
///
/// ## Heap
///
/// The heap is used to store objects that are allocated dynamically.
///
/// The heap is divided into two parts:
///
/// - Objects: Used to store objects
/// - Garbage collection: Used to store data for garbage collection
///
/// ### Object
///
/// Each object is divided into two parts:
///
/// 1. Kind: Used to store the type of the object
/// 2. Data: Used to store the data of the object
///
/// ## Strings
///
/// The strings are used to store strings that are allocated dynamically.
///
/// The strings are divided into two parts:
///
/// - Data: Used to store the strings
/// - Garbage collection: Used to store data for garbage collection
///
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Memory {
    pub stack: Stack,
    pub heap: Heap,
    pub strings: Strings,
}

/// Allocator trait has to be implemented by each memory region
pub trait Allocator<T> {
    fn allocate(&mut self, obj: T) -> u64;
    fn deallocate(&mut self, index: u64);
    fn shrink(&mut self, index: u64);
    fn mark(&mut self, index: u64);
    fn realloc(&mut self, index: u64, size: u64);
    fn sweep(&mut self);
    fn sweep_shrink(&mut self);
    fn shrink_all(&mut self);
}

impl Memory {
    pub fn read_ptr(&self, ptr: Value) -> Result<Value, Errors> {
        match ptr {
            Value::ObjectPtr(id) => {
                let obj = &self.heap.objects[id as usize];
                Ok(Value::ObjectKind(obj.kind))
            }
            Value::HeapPtr { obj, offset } => {
                let obj = &self.heap.objects[obj as usize];
                Ok(obj.data[offset as usize])
            }
            Value::StackPtr(pos) => Ok(self.stack.get_value(pos as u32)),
            Value::StringPtr(id) => Ok(Value::CharPtr { obj: id, offset: 0 }),
            Value::CharPtr { obj, offset } => {
                let string = &self.strings.data[obj as usize];
                Ok(Value::Char(string.chars().nth(offset as usize).unwrap()))
            }
            Value::UserdataPtr(_) => todo!(),
            _ => Err(Errors::InvalidPointerType(ptr)),
        }
    }

    pub fn write_ptr(&mut self, ptr: Value, value: Value) -> Result<(), Errors> {
        match ptr {
            Value::ObjectPtr(id) => {
                unreachable!("ObjectPtr is read-only")
            }
            Value::HeapPtr { obj, offset } => {
                let obj = &mut self.heap.objects[obj as usize];
                obj.data[offset as usize] = value;
                Ok(())
            }
            Value::StackPtr(pos) => {
                self.stack.set_value(pos as u32, value);
                Ok(())
            }
            Value::StringPtr(id) => {
                unreachable!("StringPtr is read-only")
            }
            Value::CharPtr { obj, offset } => {
                if let Value::Char(c) = value {
                    let offset = {
                        let mut n = 0;
                        for c in self.strings.data[obj as usize].chars() {
                            if n == offset as usize {
                                break;
                            }
                            n += c.len_utf8();
                        }
                        n
                    };
                    let (s1, s2) = self.strings.data[obj as usize].split_at(offset);
                    let s2 = &s2[1..];
                    self.strings.data[obj as usize] = format!("{}{}{}", s1, c, s2);
                    Ok(())
                } else {
                    Err(Errors::ExpectedGot(Value::Char(0 as char), value))
                }
            }
            Value::UserdataPtr(_) => todo!(),
            _ => Err(Errors::InvalidPointerType(ptr)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Stack {
    pub data: Vec<Value>,
    pub frames: Vec<StackFrame>,
    pub frame_pointer: u64,
}

impl Stack {
    /// Get the value at the given position on the current stack frame
    pub fn get_value(&self, pos: u32) -> Value {
        let frame = &self.frames[self.frame_pointer as usize];
        self.data[frame.start as usize + pos as usize]
    }

    /// Get 2 values at the given positions on the current stack frame
    /// Returns a tuple of the values
    ///
    /// This is an optimization for getting 2 values at the same time
    pub fn get_2_values(&self, pos1: u32, pos2: u32) -> (Value, Value) {
        let frame = &self.frames[self.frame_pointer as usize];
        (
            self.data[frame.start as usize + pos1 as usize],
            self.data[frame.start as usize + pos2 as usize],
        )
    }

    /// Set the value at the given position on the current stack frame
    pub fn set_value(&mut self, pos: u32, value: Value) {
        let frame = &self.frames[self.frame_pointer as usize];
        self.data[frame.start as usize + pos as usize] = value;
    }

    /// Set 2 values at the given positions on the current stack frame
    ///
    /// This is an optimization for setting 2 values at the same time
    pub fn set_2_values(&mut self, pos1: u32, pos2: u32, value1: Value, value2: Value) {
        let frame = &self.frames[self.frame_pointer as usize];
        self.data[frame.start as usize + pos1 as usize] = value1;
        self.data[frame.start as usize + pos2 as usize] = value2;
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct StackFrame {
    pub start: u64,
    pub end: u64,
    pub function_id: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Heap {
    /// Objects stored on the heap
    pub objects: Vec<Object>,

    // -- Garbage collection --
    /// Objects marked for garbage collection
    pub marked: Vec<u64>,
    /// Objects freed by garbage collection that could be reused
    pub freed: Vec<u64>,
}

impl Allocator<Object> for Heap {
    /// Allocate a new object on the heap
    fn allocate(&mut self, obj: Object) -> u64 {
        match self.freed.pop() {
            Some(id) => {
                self.objects[id as usize] = obj;
                id
            }
            None => {
                self.objects.push(obj);
                self.objects.len() as u64 - 1
            }
        }
    }

    /// Deallocate an object on the heap
    fn deallocate(&mut self, index: u64) {
        self.freed.push(index);
        self.objects[index as usize].data.clear();
        self.objects[index as usize].freed = true;
    }

    /// Shrink an object on the heap
    ///
    /// This is used to shrink the data of an object to the minimum size
    /// which is useful for objects that have been deallocated and persisted
    /// for reuse.
    fn shrink(&mut self, index: u64) {
        self.objects[index as usize].data.shrink_to_fit();
    }

    /// Mark an object on the heap
    ///
    /// This is used to mark an object for garbage collection
    fn mark(&mut self, index: u64) {
        if !self.marked.contains(&index) {
            self.marked.push(index);
        }
    }

    /// Reallocate an object on the heap
    fn realloc(&mut self, index: u64, size: u64) {
        self.objects[index as usize]
            .data
            .resize(size as usize, Value::Void);
    }

    /// Sweep the heap
    fn sweep(&mut self) {
        while let Some(index) = self.marked.pop() {
            self.deallocate(index);
        }
    }

    /// Sweep as well as shrink the heap
    ///
    /// This is less efficient than `sweep` but is useful for
    /// reducing memory usage.
    fn sweep_shrink(&mut self) {
        while let Some(index) = self.marked.pop() {
            self.deallocate(index);
            self.shrink(index);
        }
    }

    /// Shrinks the heap to the minimum size
    ///
    /// Use this after garbage collection to reduce memory usage
    fn shrink_all(&mut self) {
        if self.freed.is_empty() {
            return;
        }
        // find first object that is not freed from the end
        let mut max = 0;
        for (i, obj) in self.objects.iter().enumerate().rev() {
            if !obj.freed {
                max = i + 1;
                break;
            }
        }
        self.objects.truncate(max);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Object {
    pub kind: ObjectKind,
    pub data: Vec<Value>,
    pub freed: bool,
}

impl Object {
    pub fn new(kind: ObjectKind, size: u64) -> Object {
        Object {
            kind,
            data: vec![Value::Void; size as usize],
            freed: false,
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub struct ObjectKind {
    pub kind: ObjectKinds,
    pub id: u64,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ObjectKinds {
    Class = 0,
    Array = 1,
    Tuple = 2,
    Singleton = 3,

    Unknown = 255,
}

impl std::fmt::Display for ObjectKinds {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ObjectKinds::Class => write!(f, "Class"),
            ObjectKinds::Array => write!(f, "Array"),
            ObjectKinds::Tuple => write!(f, "Tuple"),
            ObjectKinds::Singleton => write!(f, "Singleton"),
            ObjectKinds::Unknown => write!(f, "Unknown"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Strings {
    /// Strings stored in the VM
    pub data: Vec<String>,

    // -- Garbage collection --
    /// Strings marked for garbage collection
    pub marked: Vec<u64>,
    /// Strings freed by garbage collection that could be reused
    pub freed: Vec<u64>,
}

impl Allocator<String> for Strings {
    /// Allocate a new string on the heap
    fn allocate(&mut self, obj: String) -> u64 {
        match self.freed.pop() {
            Some(id) => {
                self.data[id as usize] = obj;
                id
            }
            None => {
                self.data.push(obj);
                self.data.len() as u64 - 1
            }
        }
    }

    /// Deallocate a string on the heap
    fn deallocate(&mut self, index: u64) {
        self.freed.push(index);
        self.data[index as usize].clear();
    }

    /// Shrink a string on the heap
    ///
    /// This is used to shrink the data of a string to the minimum size
    /// which is useful for strings that have been deallocated and persisted
    /// for reuse.
    fn shrink(&mut self, index: u64) {
        self.data[index as usize].shrink_to_fit();
    }

    /// Mark a string on the heap
    ///
    /// This is used to mark a string for garbage collection
    fn mark(&mut self, index: u64) {
        if !self.marked.contains(&index) {
            self.marked.push(index);
        }
    }

    /// Reallocate a string on the heap
    fn realloc(&mut self, index: u64, size: u64) {
        self.data[index as usize].reserve(size as usize);
    }

    /// Sweep the heap
    fn sweep(&mut self) {
        while let Some(index) = self.marked.pop() {
            self.deallocate(index);
        }
    }

    /// Sweep as well as shrink the heap
    ///
    /// This is less efficient than `sweep` but is useful for
    /// reducing memory usage.
    fn sweep_shrink(&mut self) {
        while let Some(index) = self.marked.pop() {
            self.deallocate(index);
            self.shrink(index);
        }
    }

    /// Shrinks the heap to the minimum size
    ///
    /// Use this after garbage collection to reduce memory usage
    fn shrink_all(&mut self) {
        if self.freed.is_empty() {
            return;
        }
        // find first object that is not freed from the end
        let mut max = 0;
        for (i, _) in self.data.iter().enumerate().rev() {
            if !self.freed.contains(&(i as u64)) {
                max = i + 1;
                break;
            }
        }
        self.data.truncate(max);
    }
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
    pub size: u64,
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct Module {
    pub functions: Vec<Function>,
    pub classes: Vec<Class>,
    pub closures: Vec<Closure>,
    pub arrays: Vec<u64>,
    pub tuples: Vec<u64>,
}

impl Module {
    pub fn get_size(&self, kind: &ObjectKind) -> u64 {
        match kind.kind {
            ObjectKinds::Class => self.classes[kind.id as usize].size,
            ObjectKinds::Array => self.arrays[kind.id as usize],
            ObjectKinds::Tuple => self.tuples[kind.id as usize],
            ObjectKinds::Singleton => 1,
            ObjectKinds::Unknown => 1,
        }
    }
}

/// Stack position
///
/// This value is assumed to be relative to the start of the stack frame.
/// Describes an address in the stack.
pub type StackPos = u32;

/// Stack value
///
/// This value is assumed to be relative to the start of the stack frame.
/// Describes a value in the stack.
pub type StackVal = u32;

/// Unique identifier
///
/// This value is used to uniquely identify objects in the VM.
pub type ID = u64;

pub type InstrAddr = u64;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Instructions {
    /// debug stack_pos | Prints the value at stack_pos in debug mode
    Debug(StackVal),
    /// end | Ends the execution of the program
    End,
    /// abort msg | Aborts the execution of the program with the given message
    Abort(AbortMsg),

    /// write_const value stack_pos | Writes the constant value at stack_pos
    WriteConst(Value, StackPos),
    /// write_stack stack_pos1 stack_pos2 | Swaps the values at stack_pos1 and stack_pos
    Swap(StackPos, StackPos),
    /// move stack_pos1 stack_pos2 | Moves the value at stack_pos1 to stack_pos2
    Move(StackPos, StackPos),
    /// read_ptr stack_pos1 stack_pos2 | Reads the value at stack_pos1 and stores the result at stack_pos2
    ReadPtr(StackVal, StackPos),
    /// write_ptr stack_pos1 stack_pos2 | Writes the value at stack_pos1 to the address at stack_pos2
    WritePtr(StackVal, StackVal),

    /// add stack_pos1 stack_pos2 stack_pos3 | Adds the values at stack_pos1 and stack_pos2 and stores the result at stack_pos3
    Add(StackVal, StackVal, StackPos),
    /// sub stack_pos1 stack_pos2 stack_pos3 | Subtracts the value at stack_pos2 from the value at stack_pos1 and stores the result at stack_pos3
    Sub(StackVal, StackVal, StackPos),
    /// mul stack_pos1 stack_pos2 stack_pos3 | Multiplies the values at stack_pos1 and stack_pos2 and stores the result at stack_pos3
    Mul(StackVal, StackVal, StackPos),
    /// div stack_pos1 stack_pos2 stack_pos3 | Divides the value at stack_pos1 by the value at stack_pos2 and stores the result at stack_pos3
    Div(StackVal, StackVal, StackPos),
    /// mod stack_pos1 stack_pos2 stack_pos3 | Calculates the modulus of the value at stack_pos1 by the value at stack_pos2 and stores the result at stack_pos3
    Mod(StackVal, StackVal, StackPos),
    /// eq stack_pos1 stack_pos2 stack_pos3 | Compares the values at stack_pos1 and stack_pos2 and stores the result at stack_pos3
    Eq(StackVal, StackVal, StackPos),
    /// neq stack_pos1 stack_pos2 stack_pos3 | Compares the values at stack_pos1 and stack_pos2 and stores the result at stack_pos3
    Neq(StackVal, StackVal, StackPos),
    /// gt stack_pos1 stack_pos2 stack_pos3 | Compares the value at stack_pos1 with the value at stack_pos2 and stores the result at stack_pos3
    Gt(StackVal, StackVal, StackPos),
    /// gte stack_pos1 stack_pos2 stack_pos3 | Compares the value at stack_pos1 with the value at stack_pos2 and stores the result at stack_pos3
    Gte(StackVal, StackVal, StackPos),
    /// lt stack_pos1 stack_pos2 stack_pos3 | Compares the value at stack_pos1 with the value at stack_pos2 and stores the result at stack_pos3
    Lt(StackVal, StackVal, StackPos),
    /// lte stack_pos1 stack_pos2 stack_pos3 | Compares the value at stack_pos1 with the value at stack_pos2 and stores the result at stack_pos3
    Lte(StackVal, StackVal, StackPos),
    /// and stack_pos1 stack_pos2 stack_pos3 | Logical AND of the values at stack_pos1 and stack_pos2 and stores the result at stack_pos3
    And(StackVal, StackVal, StackPos),
    /// or stack_pos1 stack_pos2 stack_pos3 | Logical OR of the values at stack_pos1 and stack_pos2 and stores the result at stack_pos3
    Or(StackVal, StackVal, StackPos),
    /// not stack_pos1 stack_pos2 | Logical NOT of the value at stack_pos1 and stores the result at stack_pos2
    Not(StackVal, StackPos),

    /// index stack_pos1 stack_pos2 stack_pos3 | Indexes the value at stack_pos1 with the value at stack_pos2 and stores the result at stack_pos3
    Index(StackVal, StackVal, StackPos),

    /// allocate_static size stack_pos | Allocates a block of memory of size bytes on the heap and stores the address at stack_pos
    AllocStatic(u64, StackPos),
    /// allocate_dynamic size_stack_pos stack_pos | Allocates a block of memory of size size_stack_pos bytes on the heap and stores the address at stack_pos
    AllocDynamic(StackVal, StackPos),
    /// allocate_known kind stack_pos | Allocates a block of memory of the given kind and stores the address at stack_pos
    AllocKnown(ObjectKind, StackPos),
    /// deallocate stack_pos | Deallocates the block of memory at the address stored at stack_pos
    Dealloc(StackVal),
    /// realloc size_stack_pos stack_pos | Reallocates the block of memory at the address stored at stack_pos to size size_stack_pos bytes
    Realloc(StackVal, StackVal),

    /// reserve id | Reserves new stack frame for a function with the given id
    Reserve(ID),
    /// release | Releases the current stack frame
    Release,
    /// jump id | Jumps to the start of the function with the given id
    Jump(ID),
    /// return | Returns from the current function
    Return,
    /// return_val stack_pos offset | Puts the value at stack_pos at the end - offset of the previous stack frame
    ReturnVal(StackVal, u32),
    /// procedure lib_id func_id | Calls the function with the given id from the library with the given id
    Procedure(ID, ID),

    /// goto addr | Jumps to the instruction at addr
    Goto(InstrAddr),
    /// branch value addr1 addr2 | Branches to addr1 if value is true, otherwise branches to addr2
    Branch(StackVal, InstrAddr, InstrAddr),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum AbortMsg {
    Unreachable,
    Unimplemented,
    Panic,
}

#[derive(Clone, Copy, Serialize, Deserialize, PartialEq)]
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
        /// Unique identifier for the closure descriptor
        id: u64,
        /// Location of the closure on the heap
        captured: u64,
    },

    // -- Object kind  --
    /// Object kind
    ObjectKind(ObjectKind),

    // -- Special types --
    Void,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Char(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Uint(v) => write!(f, "{}", v),

            Value::ObjectPtr(v) => write!(f, "ObjectPtr({})", v),
            Value::HeapPtr { obj, offset } => write!(f, "HeapPtr({}, {})", obj, offset),
            Value::StackPtr(v) => write!(f, "StackPtr({})", v),
            Value::StringPtr(v) => write!(f, "StringPtr({})", v),
            Value::CharPtr { obj, offset } => write!(f, "CharPtr({}, {})", obj, offset),
            Value::UserdataPtr(v) => write!(f, "UserdataPtr({})", v),

            Value::FunctionIdent(v) => write!(f, "FunctionIdent({})", v),
            Value::ClassIdent(v) => write!(f, "ClassIdent({})", v),
            Value::ClosureIdent { id, captured } => write!(f, "ClosureIdent({}, {})", id, captured),

            Value::ObjectKind(v) => write!(f, "ObjectKind({}, {})", v.kind, v.id),

            Value::Void => write!(f, "Void"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "Int({})", v),
            Value::Float(v) => write!(f, "Float({})", v),
            Value::Char(v) => write!(f, "Char({})", v),
            Value::Bool(v) => write!(f, "Bool({})", v),
            Value::Uint(v) => write!(f, "Uint({})", v),

            Value::ObjectPtr(v) => write!(f, "ObjectPtr({})", v),
            Value::HeapPtr { obj, offset } => write!(f, "HeapPtr({}, {})", obj, offset),
            Value::StackPtr(v) => write!(f, "StackPtr({})", v),
            Value::StringPtr(v) => write!(f, "StringPtr({})", v),
            Value::CharPtr { obj, offset } => write!(f, "CharPtr({}, {})", obj, offset),
            Value::UserdataPtr(v) => write!(f, "UserdataPtr({})", v),

            Value::FunctionIdent(v) => write!(f, "FunctionIdent({})", v),
            Value::ClassIdent(v) => write!(f, "ClassIdent({})", v),
            Value::ClosureIdent { id, captured } => write!(f, "ClosureIdent({}, {})", id, captured),

            Value::ObjectKind(v) => write!(f, "ObjectKind({}, {})", v.kind, v.id),

            Value::Void => write!(f, "Void"),
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ValueKind {
    Primitive,
    Pointer,
    Identifier,
    ObjectKind,

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

            Value::ObjectKind(_) => ValueKind::ObjectKind,

            Value::Void => ValueKind::Void,
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ProgramReturn {
    Ok,
    Abort(AbortMsg),
    Error(Errors),

    Unkown,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Errors {
    InvalidPointerType(Value),
    ExpectedGot(Value, Value),
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn first_test() {
        let mut ctx = Context::new();
        let module = Module {
            functions: vec![Function {
                id: 0,
                name: "main".to_string(),
                position: 0,
                stack_size: 3,
            }],
            ..Default::default()
        };
        let instrs = vec![
            Instructions::WriteConst(Value::Int(10), 0),
            Instructions::WriteConst(Value::Int(20), 1),
            Instructions::Add(0, 1, 2),
            Instructions::Debug(2),
            Instructions::End,
        ];
        let frame = StackFrame {
            start: 0,
            end: 3,
            function_id: 0,
        };
        let stack = vec![Value::Void; 3];
        ctx.memory.stack.data = stack;
        ctx.module = module;
        ctx.executor.instructions = instrs;
        ctx.memory.stack.frames.push(frame);
        let result = ctx.run();
        assert_eq!(result, ProgramReturn::Ok);
    }
}
