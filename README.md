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
if [[ true ]] {
    a = a*(a*5);
}

var b = 13;

if [[ a == 2000 ]] {
    b = 13*2;
    a = a + b;
}

if [[ a == 2026 ]] {
    res = "Happy new year " + a + "!";
}

// while loops (now come with 50% less crashes!)
var iter = 0;

while [[ iter < 10 ]] {
    print res;
    iter = iter + 1;
}

// print for now is a keyword [blame the book not me]
print res;

// This will error!
// res = "I'm changing an immutable value";
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
