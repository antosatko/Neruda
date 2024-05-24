use runtime::{memory::Value, module::{Function, Line, Type, Types}, Context, Instructions};



fn main() {
    let mut context = Context::default();
    context.instructions = vec![
        // main
        // var 0 = 10
        Instructions::Load {
            value: Value::Int(40),
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

    let mut thread = context.create_thread();


    let value = thread.run(0).unwrap();

}
