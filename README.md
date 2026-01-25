# The Dairy Lang!

Language interpreter built in Rust, aimed at creating the worst language ever!

----

### Implementation Details
*i'll add this soon*

---

### Sample code

```dh
// Declare a mutable variable;
var a = 20;

// Declare an immutable variable;
// All uninitialized variables ar initialized as Nil
val res;

// ifs!
if true {
    a = a*(a*5);
}

var b = 13;

if a == 2000 {
    b = 13*2;
    a = a + b;
}

if a == 2026 {
    res = "Happy new year " + a + "!";
}

// while loops (now come with 50% less crashes!)
var iter = 0;

while iter < 10 {
    print res;
    iter = iter + 1;
}

// print for now is a keyword [blame the book not me]
print res;

// This will error!
// res = "I'm changing an immutable value";

// types
var i_am_str: Str = "Hello, world!";

// interpreter has builtin type checking so this will error
// i_am_str = 1;

// type deduction
var i;
i = 1; // type of i becomes number

// lists
var ls = [1,2,3];

// type annotated lists
var ls: [[Number]] = [[1,2,3], [4,5,6], [7,8,9]];

// lists can be accessed with both positive and negative indicies
print "" + ls[1][-1] + ls[-1][0];

// range and long arrow operator
// for easy looping from start to end - 1 there is a long arrow operator which generatos a range of numbers
var range: Range = 1-->11;

// for loops. i added pythonic for in loops for simplicity 
for i in range 
  print i;

var str = "";
for c in "hello" {
    str = str + c;
}

print str + ", world!";

for i in [1,2,3] {
    print i;
}

```

--- 

### Releases

- ![Alpha 1 - added If statements](https://github.com/nodairyco/rs_dairy_lang/releases/tag/V-0.0.1)
- ![Alpha 2 - added While statements, and some operators](https://github.com/nodairyco/rs_dairy_lang/releases/tag/Alpha-2)
- ![Alpha 3 - fixed bug with while loops crashing on high iterations](https://github.com/nodairyco/rs_dairy_lang/releases/tag/Alpha-3)

--- 

### Running the interpreter
Either use a provided binary, or
```bash
git clone https://github.com/nodairyco/rs_dairy_lang/tree/master &&
cargo run
```
Running without arguments opens Dairy Shell, a REPL of sorts. Running with a file name runs that file.
