# Zigdynamo
Implementation of very basic dynamic programming language which I am creating purely for experimentation in programming language implementation. Because of that the design decisions are not made by careful analysis of what would be the best fit for creating useful language but by impulsive decisions on what shit I want to implement.

## Usage
The only dependency it has is test dependency so you can use the normal zig build system. I only run this on linux x86\_64 machine though so your mileage may very but running jit on arm should be fun (segfault i hope).

```bash
# bytecode intepreter
zig build run -- --bc examples/linkedlist.dyn

# bytecode intepreter with jit (surely this works on arm - please dont try)
zig build run -- --jit examples/linkedlist.dyn

# write out textual representation of bytecode
zig build run -- --cmp examples/linkedlist.dyn
```

## Testing
`zig build test` but only frontend and bytecode compiler is tested a bit. The decision making  framework proliferates the whole project as you can see.

## Example of program
You can see more examples in examples directory

```
let pos = object {
    x: 1,
    y: 2,
};

let person = object {
    name: "Adam",
    age: 25,
    ageup: fn() = {
        this.age = this.age + 1;
    },
};

let ask = fn (person) = {
    let age = person.age;
    if (age < 18)
        print("cannot drink")
    else 
        print("can drink");
};

ask(person);
person.age = 15;
ask(person);
person.ageup();
print(person.age);
```

## Features
The feature set is very minimal to allow easier playing with the implementation, maybe expanded if I would be interested in implementation of some other feature (SSO comes to mind)

* classic numeric (32bit) and usual operations
* standard conditionals and looping (no for loops because I could not be bothered in the parser)
* functions with closures which are treated as values
* object with prototypal inheritance
* the global environment with dynamic scope and local scopes with local scopes
* strings only constant and immutable => you can only have string which you write in source code

## Implementation
The implementation is done in zig without other dependencies other then snapshot testing framework ohsnap. The basic split is traditional for implementation of programming languages and that is lexer, parser, compiler and vm that runs the bytecode (there is AST interpreter but its is broken now).

### Lexer and parser
LL parser with recursive descent implementation. TODO: say more

### Bytecode
Instruction set is standard stack based vm machine bytecode which can interact with the constant pool. The textual representation of compiled bytecode can be seen with `--cmp` flag on command line.

The bytecode can by split into constant as you can see in the example, the first constant is considered the entry point of program. 

```
main_function (26 bytes)
	5: closure 0 0 0 5 0 0 0 0
	14: set_global 0 0 0 0
	19: pop
	20: push_byte 1
	22: get_global_small 0
	24: call
	25: ret_main

string (6 bytes)
string: n

string (10 bytes)
string: greet

string (7 bytes)
string: yo

class (13 bytes)
class: 1 2
function (34 bytes)
	17: nil
	18: get_small 0
	20: push_byte 1
	22: add
	23: string 0 0 0 3
	28: object 0 0 0 4
	33: ret
```

### JIT
TODO



