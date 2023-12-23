# `mut`
```rust
let mut x: i32 = 1;
    x = 7;
    // Shadowing and re-binding
    let x = x; 
    x += 3;
```
redaclaring x makes it immutable.

# `assert_eq`

# shadowing (redeclaring)

# `let _x`
for no warnings. for unused variable

or 

# `#[allow(unused_variables)]`

# destructuring `let (mut x, y) = (1, 2);`

# `i8, i16, i32, i64` `u8, i16, u32, u64`

**8bit** unsigned minimum: 0
```
 0   0   0   0   0   0   0   0
2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0
```

**8bit** unsigned maximum: 255
```
 1   1   1   1   1   1   1   1
2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0
```

`i8`  min:-128             max:127
`u8`  min:0                max:255
`i32` min:-2,147,483,648   max:2,147,483,647
`u32` min:0                max:4,294,967,295    

1 byte = `0000 0100`  (8 bits)

processor does not read 1 byte at the time, it read 1 word at the time.
32-bit processor can read 4 bytes (32-bits) at the time.
64-bit processor can read 8 bytes (64-bits) at the time.

# `38_u8`
this assines a type straight to a value.

# `as`
converts a type as another type:
```rust
let v: u16 = 38_u8 as u16;
```

# `1_024 + 0xff + 0o77 + 0b1111_1111;` == 1579
operations on different base systems are possible.
Decimal + Hex + Oct + binary.

# ranges `-5..2` `-5..=2` `'a'..= 'z'`

# `a as u8` => `97` ASCII code for a  

# `1<<5` `128>>2`
moving the bit
```
0 0 0 0 0 0 0 1`
1<<5
0 0 1 0 0 0 0 0
```

```
1 0 0 0 0 0 0 0
0x80>>2 = 128>>2
0 0 1 0 0 0 0 0
```

# `0b0011u32 & 0b0101` = `0001`
AND, OR XOR
`0b0011u32 | 0b0101` = `0101`
`0b0011u32 ^ 0b0101` = `0001`

#  `use std::mem::size_of_val`
`size_of_val('a') == 4`
4 bytes

# Unit Type `()`
holds zero bytes. If a function has no return, it will implicitly return `()`.

# Char = 4 bytes | Bool = 1 Byte | Unit = 0 bytes

# `{2 * x}` returns x*2, `{x = 2;}` with semicolon it returns `()`
is the difference between expression and statement.
`x += 2` is a statement even if we don't put semicolon `;`
to return x we need to say it after
```rust
{
  x +=2; // necessary ;
  x
}
```

# `fn never_return() -> ! { panic!() }`  Diverging Function

# `match` is like a switch
```rust
match tp {
  1 => {
    // TODO
  }
  _ => {
    // TODO
  }
};
```

# `todo!()` `panic!()` `unimplemented!()` are all macros

# OWNERSHIP
Who is the owner of the Data?

is a set of rules that govern memory management.
The rules are enforced at compile time. If not it won't compile.

Each value as an owner. There is only one owner per time.
When the owner goes out of scope, the value is dropped.

# Global Scope and Local Scope
Global means variable accessible in the entire program.
Local is accessible ony in a block of code or function.

# Memory
is a componenet in the computer to store data and instructions for processor to execute.
This memory is volatile, RAM, and goes away once computer is off.
RAM has two regions:

1. Stack Memory
  Use when the size is known (at compile time)
  Last in, First out (like a stack of dishes)
  data in the stack must have known fixed size.
  Stack is faster integers,char,bools are stored there.
  ```
  main() {x,y,z}
  fn(a, b) {a,b,c}
  ```
  in the stack:
  ```
  ----------
  fn()
  x,y,z
  ----------
  main()
  a,b,c
  ----------
  ```
  When the porgram runs, it will start from the top (fn)

1. Heap Memory
  When values have unkown size (at compile time) they'll be pushed in the heap
  with a **pointer** to that heap value, pointer that will be
  allocated to the stack, because that pointer has fixed size

a string is not of fixed size and it will be allocated to the heap memory with a pointer store in the stack.
`let s1 = string::from("hello");`
`s1` doesn't hold "hello" it holds the pointer to the heap memory part that holds "hello".
## `usize`
`s1` will hold three values, the pointer, the length, and the capacity, 1 byte each, for an *usize* of 24 bytes (3 * 8 byte). A knows size to put in the stack memory.
Once you know location, and the length, you have all you need to fish this data from the heap memory.

## Copy Vs Move
Copy is for the stack where everything is cheap.
Move is for the heap where copying would be too expensive.
```rust
let x = 5
let y = x
```
`x` will be copied because it was in the stack memory it being a known size as a `i32` type. Two different locations (x,y) for hte same value (5).
```rust
let s1 = string::from("hello")
let s2 = s1
```
`s2` gets a copy of the pointer not the actual data. `s1` and `s2` now point to the same data in the heap memory.
But that violates the ownrship rule that there can only be one owner at each time. So Rust at this point will **drop** `s1`.
`s2` is now the sole owner of `"hello"` in the heap memory.

# Deep Copy
if we actually want to have a copy, we need to be explicit.
```rust
let s1 = String::from("hello");
let s2 = s1.clone();  
```
this is because is expensive to have data in the heap, so it is made it difficult to multiply data in it.

# Ownership and Functions
example 1:
```rust
fn main() {
  let s = String::from("hello"); // s comes into scope

  takes_ownership(s);            // s' value moves into the function..
                                 // ..and is no longer valid here

  let x = 5;                     // x comes into scope

  makes_copy(x);                 // x would move into the function
                                 // but the i32 is copied, so it is still fine to use x afterward 
} // Here x goes out of scope, then s would go out of scope, but since it was moved (ownership), nothing special happens on s.

fn takes_ownership(some_string: String) { // some_string comes into scope
  println!("{}", some_string);
} // here some_string goes out of scope and `drop` is called. Backing memory is freed

fn makes_copy(some_integer: i32) {
  println!("{}", some_integer);
} // here some_integer goes out of scope. Nothing special happens.
```
example 2:
```rust
fn main() {
  let s1 = gives_ownership();           // gives_ownership moves its return value into s1

  let s2 = String:from("hello");        // s2 comes into scope

  let s3 = takes_and_gives_back(s2);    // s2 is moved into takes_and_gives_back which moves its
                                        // return value into s3
} // Here s3 goes out of scope and is dropped. s2 was moved, so nothing happens. s1 goes out of scope
  // and is dropped.

fn gives_ownership() -> String {                 // gives_ownership will move its return value
                                                 // into the function that calls it

  let some_string = String::from("yours");       // some string comes into scope

  some_string                                    // some string it's returned and moves out to
                                                 // the calling function
}

fn takes_and_gives_back(a_string: String) -> String {  // a string comes into scope
  a_string                        // a string is returned and moves out to the calling function 
}
```

All of this shenanigans about are useful for safety, so that you don't end up with dangling pointers, double free and memory leaks.

# `.clone()`
```rust
  let x = String::from("hello, world");
  let y = x;               // x went out of scope here because moved into y
  println!("{}, {}",x,y);  // so it is not found here anymore
```

```rust
  let x = String::from("hello, world");
  let y = x.clone();       // we now make a deep copy of x.
  println!("{}, {}",x,y);  // and it can now be called.
```

# `Box:new(5)`
to allocate to the heap what would have gone to the stack.

# `*y` derefencing
```rust
let mut y: Box::<i32> = Box::new(1);

*y = 4;    // without the * derefencing you couldn't have assigned a type stack i32 to a variable with type heap Box<>
```

the `*` makes you access the value and not the pointer.

```rust
let x: Box::<i32> = Box::new(5);

assert_eq!(*x, 5);    // without the * the assert wouldn't have worked, because it would have compared a pointer
                      // to a stack i32, accessing the value in the heap with * it works.
                      // x == 0x34950sd34  (a memory address)
                      // *x == 5           (a i32)
```

# `ref`
```rust
fn main() {
    #[derive(Debug)]
    struct Person {
        name: String,
        age: Box<u8>,
    }

    let person = Person {
        name: String::from("Alice"),
        age: Box::new(20),
    };

    // `name` is moved out of person, but `age` is referenced
    let Person { name, ref age } = person; // reference means it points to the age in person.

    println!("The person's age is {}", age);

    println!("The person's name is {}", name);

    // Error! borrow of partially moved value: `person` partial move occurs
    //println!("The person struct is {:?}", person);

    // `person` cannot be used but `person.age` can be used as it is not moved
    println!("The person's age from person struct is {}", person.age);
}
```

# tuple access `t = ("a", "b")` `t.0` = `"a"`

## ownership and tuples
```rust
let t = (String::from("hello"), String::from("world"));

  let _s = t.0;

  // Modify this line only, don't use `_s`
  // println!("{:?}", t); wouldn't work.
  println!("{:?}", t.0);
```
```rust
fn main() {
   let t = (String::from("hello"), String::from("world"));

    // Fill the blanks
    let (s1, s2) = t.clone(); // if not clone wouldn't work.

    println!("{:?}, {:?}, {:?}", s1, s2, t); // -> "hello", "world", ("hello", "world")
}
```

# BORROWING

Temporarily accessing data, **without taking ownership** of it.

It will take a reference (pointer) to the data and not the data itself.

Prevents dangling pointers. Can be borrowed immutably or mutably.

There are rules for a successful compiler:
1. At any given time, you can **either** have **one mutable reference** or **any number of immutable references**
2. References must **always be valid**

# Reference `&`
example:
```rust
fn main() {
  let s1 = String::from("hello");    // s1 is a pointer to the heap memory that holds the actual value

  let len = calculate_length(&s1);   // s (in calculate_length) points to the pointer s1 that points to the 
                                     // heap memory actual value.

  println!("The length of '{}' is {}.", s1, len);
}

fn calculate_length(s: &String) -> usize {
  s.len()
}
```
in this way `s1` remains the owner of the allocated Heap memory for the string value, even while `s` gets a pointer to `s1` itself

# Mutable Reference `&mut`
```rust
fn main() {
  let mut s = String::from("hello");

  change(&mut s);
}

fn change(some_string: &mut String) {
  some_string.push_str(", world");
}
```
this example is ok because we have only one mutable reference to the data `s`.

in this example we would fail, because the first rule isn't respected:
```rust
let mut s = String::from("hello");

let r1 = &mut s;
let r2 = &mut s;  // second mutable reference, is not allowed.

println!("{}, {}", r1, r2);
```
you can only have many IMmutable references.

```rust
let mut s = String::from("hello");

{
  let r1 = &mut s;
} // r1 goes out of scope here, so we can make a new reference with no problems.

let r2 = &mut s;
```

```rust 
let mut s = String::from("hello");

let r1 = &s; // no problem (immutable)
let r2 = &s; // no problem (immutable)
let r3 = &mut s; // BIG PROBLEM (mutable! after other immutable.)

println!("{} {} {}",r1,r2,r3);
```

```rust
let mut s = String::from("hello");

let r1 = &s; // no problem (immutable)
let r2 = &s; // no problem (immutable)

println!("{} {}",r1,r2);
// variables r1 and r2 will not be used after this point

let r3 = &mut s;
println!("{}",r3); //this works.
```

# Dangling References, violating 2nd rule of references.
second rule: references must always be valid!
```rust 
fn main() {
  let reference_to_nothing = dangle();
}

fn dangle() -> String {
  let s = String::from("hello");

  &s   // we are returning to the string s which will be dropped after this func
       // that's what it means by dangling references a garbage value
}
```
rust won't allow you to compile something like the above.

```rust
fn no_dangle() -> String {
  let s = String::from("hello");
  s
}
```

# `println("{:p}",p)` `:p`
`:p` writes the memory allocation that the pointer is holding. 
A pointer is just a value that holds a memory address.
```rust
fn main() {
  let x: i32 = 5;
  // fill the blank
  let p: &i32 = &x;  // a pointer with a reference to the stack memory?

  println!("the memory address of x is {:p}", p); //possible output: 0x7ffc0965a37c
}
```

to access the actuall value of `y` you'd have to use the dereference operator `*`
```rust
assert_eq!(5, *y);
```
`*` says go to the location of this pointer and give me the value.

# `ref` is same as `&` 
jsut some minor differences for pattern matching

# `format!()`
like `println!()` but it returns the string.

# Two Mutable References to Same Data
it is possible as long as you don't use the first reference after you introduced a new reference
```rust
fn main() {
    let mut s = String::from("hello, ");

    let r1 = &mut s;
    r1.push_str("world");
    let r2 = &mut s;
    r2.push_str("!");
    
    println!("{}",r1); //if you comment this one out, or change r1 for r2, the code will compile.
}
```

# COMPOUND TYPES
Types that are made up from other types. 

Like a String is made up of Char types.

# `String` and `&Str` String Slice
`String` is in Heap memory and is mutable and owns its content.

`&str` is an immutable sequence of UTF-8 bytes in memory. and is not owned.

`&str` is just a view on a sequence of UTF-8 Bytes in memory.

`&str` is if you only need to view a string, it is lightweight.

`String` if you need to own data and mutate it.

# String Literal
sequence of characters enclosed in "".

`&'static str` indicating data is stored in static storage meaning that it is valid thoughout the entire lifetime of the program

it is hardcoded into the executable of stored in read-only memory, meaning they are immutable.

basically is every time you use `"anything"` in your code.

```rust
let s: &str = "hello, world";
```
`"hello, world"` is hard coded in the static memory, we can use a reference (`&str`) to that string (or str).

To get a `String` instead we would do:
```rust
let s: String = String::from("hello, world");
```

at this point `s` is a pointer that has a memory location that points at the heap memory where "hello, world" is located and a len that tells it how many slot of memory (char) it needs to take (12).

```json
s: {
  ptr: "0x...",
  len: 12
}

heap: {
  ..,
  "h",  //0x..
  "e",
  "l",
  "l",
  "o",
  ","
  " ",
  "w",
  "o",
  "r",
  "l",
  "d"
  ..,
}
```

can also be the whole string and still be a string slice:


# `s.as_str()` is `String -> &str`
Needed for  `s1 + s.as_str();`.

Can Also be achieved as `s1 + &s;`

# `s.to_string()` to covnert `&str` to `String`
`String::from(s)` works the same.

# Hexadecimal Values Writing
You can use escapes to write bytes by their hexadecimal values
```rust
"string.. \73 -tring \\73" // string.. s -tring \73
```

```rust
let unicode_codepoint = "\u{211D}";
let character_name = "\"DOUBLE-STRUCK CAPITAL R\"";
```

# String index
```rust
let s1 = String::from("hi,中国");
let h = &s1[0..1];                 // h
```

# `.chars()`
creates an iterator to go through the characters of a string.

# ARRAYS
Fixed size of same type elements goes in to the **Stack Memory**

its type is `[T; Lenght]` so the lenght is known at compile time.

It can not be shrunk or grown.

```rust
fn init_arr(n: i32) {
    let arr = [1; n];
}
```
this can't be, it won't compile.

# Array Shorthands
`[1; 6]` == `[1,1,1,1,1,1]`

# SLICE
reference to contiguous sequence of elements in a collection. 

borrows part of a collection without taking ownership of the entire collection.

Collection can be arrays, vectors, Strings and any other collection implementing `Deref`.

Similar to `&str` is a view into a collection.

```rust
let a = [1,2,3,4,5];
let slice: &[i32] = &a[1..3];
assert_eq!(slice, &[2, 3]);
```

`&arr[0..3]` can also be `&arr[..3]`

# TUPLES `(T, T, T..)`
`T` stands for type.

Can index `t.0`, `t.1`

Only up to 12 elements tuple can be printed.

Can be destructured `let (x, y, z) = tuple`

# STRUCT
A template of the instances you create from it. It creates your custom Data

```rust
struct Person {
    name: String,
    age: u8,
    hobby: String
}

let age: u8 = 30;
let p = Person {
    name: String::from("sunface"),
    age,
    hobby: String::from("coding"),
};
```

```rust
struct Color(i32, i32, i32);
struct Point(i32, i32, i32);

let v = Point(20, 25, 33);
check_point(v);

fn check_point(p: Point) {
    let Point(x, _, _) = p;  // need to provide the name of the struct
    assert_eq!(x, 20);
    assert_eq!(p.1, 25);
 }
```

You can't make mutable only a property, the whole struct must be **mutable**.

```rust
struct Person {
    name: String,
    age: u8,
} 

fn build_person(name: String, age: u8) -> Person {
    Person {
        age,
        name,
    }
}
```

```rust
struct User {
    active: bool,
    username: String,
    email: String,
    sign_in_count: u64,
}

let u1 = User {
    email: String::from("someone@example.com"),
    username: String::from("sunface"),
    active: true,
    sign_in_count: 1,
};

let u2 = set_email(u1);

fn set_email(u: User) -> User {
    User {
        email: String::from("contact@im.dev"),
        ..u,
    }
}
```

Usually struct isn't printable, unless you use this flag
```rust
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

println!("{:?}", rect1);
```

# `dbg!`
macro that prints to the stderr

```rust
let rect1 = Rectangle {
        width: dbg!(30 * scale), // Print debug info to stderr and assign the value of  `30 * scale` to `width` only this property
        height: 50,
    };

dbg!(&rect1); // Print debug info to stderr ofthe whole rect1
```

# Partial Move of `struct`
```rust
#[derive(Debug)]
struct File {
    name: String,
    data: String,
}
fn main() {
    let f = File {
        name: String::from("readme.md"),
        data: "Rust By Practice".to_string()
    };

    let _name = f.name;

    println!("{}, {}, {:?}",f.name, f.data, f); // f.name can't be used, because the owner of it is now _name
    // not even f as a whole can be used any longer.
    // you can only use it partially, what has not been moved.
    println!("{}, {}", _name, f.data);
} 
```

```rust
// and easier way is to clone the variable instead
let _name = f.name.clone();

println!("{:?}",f) // f can be printed now.
```

# ENUM
Can still create custom types.

When you need to access only one variable from a set.

Usefull in match patterns

```rust
enum IpAddr {
  V4(String),
  V6(String),
}

let home = IpAddr::V4(String::from("127.0.0.1"));

let loopback = IpAddr::V6(String::from("::1"));
```
Can only be V4 or V6.
> that is probably what the `String::from()` means.

# enum can be enumerated
with integers
```rust
enum Number {
    Zero, // 0
    One,  // 1
    Two,  // 2
}

enum Number1 {
    Zero = 0,
    One, // 1
    Two, // 2
}

// C-like enum
enum Number2 {
    Zero = 5,
    One, // 6
    Two = 9,
}

println!("{}",Number2::Zero as u8) // ==> 5
```

# enum can hold data
```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 }, // like a struct.
    Write(String),
    ChangeColor(i32, i32, i32),
}

let msg1 = Message::Move{x: 1, y: 2};
let msg2 = Message::Write("hello, world!");
```

```rust
#[derive(debug)]
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

fn main() {
    let msgs: __ = [
        Message::Quit,
        Message::Move{x:1, y:3},
        Message::ChangeColor(255,255,0)
    ];

    for msg in msgs {
        show_message(msg)
    }
} 

fn show_message(msg: Message) {
    println!("{:?}", msg);
}
```

# Option Enum
An enum that represents a value that may or may not be present.
```rust
enum Option<T> {
  None,
  Some(T),
}
```

```rust
fn main() {
  let five = Some(5),
  let six = plus_one(five),
  let none = plus_one(none),
}

fn plus_one(Option<i32>) -> Option<i32> {
  match x {
    None => None,
    Some(i) => Some(i + 1), // destructuring Some first
  }
}
```

# FLOW CONTROL

## if
```rust
let n = 5;

if n < 0 {
    println!("{} is negative", n);
} else if n > 0 {
    println!("{} is positive", n);
} else {
    println!("{} is zero", n);
}
```

If/Else expressions can be used in assignments:
```rust
let n = 5;

let big_n: i32 =
  if n < 10 && n > -10 {
    println!(", and is a small number, increase ten-fold");
    10 * n
  } else {
    println!(", and is a big number, halve the number");
    n / 2.0 as i32;
  }
```

## for
```rust
let numbers = [1, 2, 3];
// The elements in numbers are Copy，so there is no move here
for n in numbers {
    // Do something with name...
}
```

```rust
let names = [String::from("liming"),String::from("hanmeimei")];
for name in &names {  //notice we reference strings
    println!("{:?}", name);
}
println!("{:?}", names);

let numbers = [1, 2, 3];
for n in numbers {
    println!("{:?}", n);
}
println!("{:?}", numbers);
```

# `.enumerate()`
```rust
let a = [4, 3, 2, 1];

// Iterate the indexing and value in 'a'
for (i,v) in a.iter().enumerate() {
    println!("The {}th element is {}",i+1,v);
}
```

## while
```rust
// A counter variable
let mut n = 1;

// Loop while the condition is true
while n < 10 {
  if n % 15 == 0 {
      println!("fizzbuzz");
  } else if n % 3 == 0 {
      println!("fizz");
  } else if n % 5 == 0 {
      println!("buzz");
  } else {
      println!("{}", n);
  }

  n += 1;
}
```

## break
```rust
for i in 0..=100 {
  if n == 66 {
    break;
  }
  n += 1;
}
```

## continue
```rust
let mut n = 0;
for i in 0..=100 {
  if n != 66 {
    n+=1;
    continue;
  }
  
  break;
}

assert_eq!(n, 66);
```

## loop
```rust
let mut count = 0u32;

// Infinite loop
loop {
  count += 1;

  if count == 3 {
    println!("three");

    // Skip the rest of this iteration
    continue;
  }

  println!("{}", count);

  if count == 5 {
    println!("OK, that's enough");

    break;
  }
}

assert_eq!(count, 5);
```

## nested loops
Need labels:
```rust
let mut count = 0;
'outer: loop {
  'inner1: loop {
    if count >= 20 {
      // This would break only the inner1 loop
      break 'inner1; // `break` is also works.
    }
    count += 2;
  }

  count += 5;

  'inner2: loop {
    if count >= 30 {
      // This breaks the outer loop
      break 'outer;
    }

    // This will continue the outer loop
    continue 'outer;
  }
}

assert!(count == 30);
```

# PATTERN MATCH
Compare a value against a set of patterns, execute different code based on matched pattern.

All possible cases must be enforced.

```rust
enum Direction {
    East,
    West,
    North,
    South,
}

fn main() {
    let dire = Direction::South;
    match dire {
        Direction::East => println!("East"),
        Direction::South | Direction::North  => { // Matching South or North here
            println!("South or North");
        },
        _ => println!("it must be West"),
    };
}
```

Match is an expression, so we can use it in assignments.
```rust
let boolean = true;

let binary = match boolean {
  true => 1,
  false => 0,
};

assert_eq!(binary, 1);

println!("Success!");
```

```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

fn main() {
    let msgs = [
        Message::Quit,
        Message::Move{x:1, y:3},
        Message::ChangeColor(255,255,0)
    ];

    for msg in msgs {
        show_message(msg)
    }

    println!("Success!");
} 

fn show_message(msg: Message) {
    match msg {
        Message::Move{x: a, y: b} => { // match  Message::Move
            assert_eq!(a, 1);
            assert_eq!(b, 3);
            println!("inside Move"); 
        },
        Message::ChangeColor(_, g, b) => {
            assert_eq!(g, 255);
            assert_eq!(b, 0);
            println!("inside ChangeColor");
        }
        _ => println!("no data in these variants")
    }
}
```

# `matches!`
```rust
let alphabets = ['a', 'E', 'Z', '0', 'x', '9' , 'Y'];

// Fill the blank with `matches!` to make the code work
for ab in alphabets {
    assert!(matches!(ab, 'A'..='Z' | 'a'..='z' | '0'..='9'))
}

println!("Success!");
```

# if let
for when `match` is too heavy.
```rust
let o = Some(7);

match o {
    Some(i) => {
        println!("string and `{:?}`", i);
    }
    _ => {}
};

//the whole `match` block, using `if let` instead
if let Some(i) = o {
    println!("string and `{:?}`", i);
}
```

# Pattens
```rust
fn match_number(n: i32) {
  match n {
    // Match a single value
    1 => println!("One!"),
    // Fill in the blank with `|`, DON'T use `..` or `..=`
    2 | 3 | 4 | 5 => println!("match 2 -> 5"),
    // Match an inclusive range
    6..=10 => {println!("match 6 -> 10")},
    _ => {println!("match -infinite -> 0 or 11 -> +infinite")}
}
```

# `@`
The `@` operator lets us create a variable that holds a value, at the same time we are testing that value to see whether it matches a pattern.
```rust
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    // Fill in the blank to let p match the second arm
    let p = Point { x: 3, y: 20 };

    match p {
        Point { x, y: 0 } => println!("On the x axis at {}", x),
        // Second arm
        Point { x: 0..=5, y: yAt@(10 | 20 | 30) } => println!("On the y axis at {}", yAt),
        Point { x, y } => println!("On neither axis: ({}, {})", x, y),
    }
}
```
It basically allow you to pattern match, while still using the matched attribute as a variable inside the matching funciton.

In other words it allows you to destructure while pattern matching.

# Match Guard
A match guard is an additional `if` condition specified after the pattern in a match arm that must also match, along with the pattern matching, for that arm to be chosen.
```rust
let num = Some(4);
let split = 5;
match num {
    Some(x) if x < split => assert!(x < split),
    Some(x) => assert!(x >= split),
    None => (),
}

println!("Success!");
```

# Ignoring in Pattern Matching
```rust
let numbers = (2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048);

match numbers {
    (first, .., last) => {
        assert_eq!(first, 2);
        assert_eq!(last, 2048);
    }
}
```

# METHODS `impl`
Function associated with a particular type/`struct`
```rust
struct Rectangle {
  width: u32,
  height: u32,
}

impl Rectangle {
  fn area(&self) -> u32 {
    self.width * self.height
  }
}
```
use:
```rust
let rect1 = Rectangle {
  width: 30,
  height: 50,
}

println!("area: {}", rect1.area())
```

# Associated Function
Associated with a `struct` or `enum` but not with an instance of them.

No `&self` but called using the name of the type not an instance.
```rust
struct Rectangle {
  width: u32,
  height: u32,
}

impl Rectangle {
  fn new(width: u32, height: u32) -> Rectangle {
    Rectangle {
      width,
      height,
    }
  }
}
```
use:
```rust
let rect1: Rectangle = Rectangle::new(5, 10);
```

It does not take an instance, because it does return an instance.

```rust
struct TrafficLight {
  color: String,
}

impl TrafficLight {
  pub fn new() -> Self { //notice capital self can be used for the type
    Self {
      color: String::from("red")
    }
  }

  pub fn get_state(&self) -> &str {
    &self.color
  }
}

fn main() {
  let light = TrafficLight::new();
  assert_eq!(light.get_state(), "red");
}
```

Multiple blocks can be used with `impl` for restructuring and organising.
```rust
struct Obj {
  // ...
}

impl Obj {
  // ...
}

impl Obj {
  // ...
}
```

You can use `impl` with `enum` too.

# GENERICS
until now we only treated with concrete types. Generics are **placeholders** for concrete types. 

Allowing for more reusable and flexible code
```rust
struct A;          // Concrete type `A`.
struct S(A);       // Concrete type `S`.
struct SGen<T>(T); // Generic type `SGen`. // T is a convention, can be different but better not.
                   // the <> is what makes it generic.
```
The generic type means that struct can hold any type.
```rust
// non-generic functions
fn reg_fn(_s: S) {}
fn gen_spec_t(_s: SGen<A>) {}
fn gen_spec_i32(_s: SGen<i32>) {}

// generic functions
fn generic<T>(_s: SGen<T>) {}

// Explicitly specified type parameter `char` to `generic()`.
generic::<char>(SGen('a'));

// Implicitly specified type parameter `char` to `generic()`.
generic(SGen('t'));
generic(SGen(7.8)));
```

```rust
fn sum<T>(a: T, b: T) -> T {
  a + b
}

assert_eq!(5, sum(2i8, 3i8));
assert_eq!(50, sum(20, 30));
assert_eq!(2.46, sum(1.23, 1.23));
```
in reality for this to work a trai should be added because we have an addition to make to work
```rust
fn sum<T: std::ops::Add<Output = T>>(a: T, b: T) -> T {
  a + b
}
```

```rust
let integer = Point { x: 5, y: 10 };
let float = Point { x: 1.0, y: 4.0 };
```
Once we use `i32` and once we use `f64`.

To make this possible we need to use generics:
```rust
struct Point<T> {
  x: T,
  y: T,
}
```
but because now we are using generics, we also need a different annotation:
```rust
let integer = Point<i32> { x: 5, y: 10 };
let float = Point<f64> { x: 1.0, y: 4.0 }; // in reality the compiler can infer it in this case
```

```rust
struct Point<T, U> {
    x: T,
    y: U,
}

let p = Point<i32, String>{x: 5, y : "hello".to_string()};
```

```rust
struct Val<T> {
  val: T,
}

impl<T> Val<T> {
  fn value(&self) -> &T {
    &self.val
  }
}

let x: Val<f64> = Val{ val: 3.0 };
let y: Val<String> = Val{ val: "hello".to_string()};
```

```rust
struct Point<T, U> {
  x: T,
  y: U,
}

impl<T, U> Point<T, U> {
        //notice we had to add types here as they are different from those next to `impl`
  fn mixup<V, W> (self, other: Point<V, W>) -> Point<T, W> {
    Point{
      x: self.x,
      y: other.y,
    }
  }
}

fn main() {
  let p1 = Point { x: 5, y: 10 };
  let p2 = Point { x: "Hello", y: '中'};

  let p3 = p1.mixup(p2);
  // wants something like: Point {x: 5, y: '中'}
  
  assert_eq!(p3.x, 5);
  assert_eq!(p3.y, '中');

  println!("Success!");
}
```

# Const Generics
types to be parameterized by integers.

```rust
struct Array<T, const N: usize> {
  data : [T; N]
}


let arrays: [Array<i32, 3>; 3] = [
  Array{ data: [1, 2, 3]},
  Array{data: [1, 2, 8]},
  Array{data: [4, 5, 6]},
];

let floats: [Array<f64, 2>; 3] = [
  Array{data: [0.3, 0.6]},
  Array{data: [0.99, 0.324]},
  Array{data: [0.13, 0.4]},
]
```

# TRAITS
Methods to be implemented for **multiple types** to provide common functionalities/behaviours between them.

Shared behaviour in an abstract way.

Traits define only the signature.

```rust
trait Animal {
  fn sound(&self) -> String;
}

struct Sheep;
struct Cow;

impl Animal for Sheep {
  fn sound(&self) -> String {
    String::from("Maah")
  }
}

impl Animal for Cow {
  fn sound(&self) -> String {
    String::from("Mooh")
  }
}
```

# `impl Traits`
Traits can be used as parameters for functions.
```rust
fn notify(item: &impl Summary) {
  println!({"breaking news: {}"}, item.summarise())
}
```
The fucntion `notify()` takes as arguments any type that has implemented the Summary trait.

# `<T: Traits>`
Similar to `impl Summary` but more verbose.
```rust
fn notify(item: &impl Summary) {
  println!({"breaking news: {}"}, item.summarise())
}
```
Is a better use if you have a lot of parameters. To avoid this:
```rust
fn notify(item1: &impl Summary, item2: &impl Summary) { //..

fn notify<T: Summary>(item1: &T, item2: &T) { //..
```

# `Trait1 + Trait2` and `where`
```rust
fn some_func<T: Display + Clone, U: Clone + Debug>(t: &T, u: &U) -> i32 {}
```

can make code cleaner by using `where`:

```rust
fn some_func<T, U>(t: &T, u: &U) -> i32 
where 
  T: Display + Clone,
  U: Clone + Debug,
{}
```

## Return Types with Traits
```rust
trait Animal {}

struct Dog;
struct Cat;

impl Animal for Dog {}
impl Animal for Cat {}

fn return_dog() -> impl Animal {
  Dog {}
}

fn return_cat() -> impl Animal {
  Cat {}
}

fn main() {
  return_dog();
  return_cat();
}
```

# Default Implementation of Traits
```rust
trait Hello {
    fn say_hi(&self) -> String {        // not just a signature (default implementation)
        String::from("hi")
    }

    fn say_something(&self) -> String;  // we'll have to implement this one instead.
}
```
here how the code would continue:
```rust
struct Student {}
impl Hello for Student {
  fn say_something(&self) -> String {   // signature will be exactly the same
    String::from("I'm a good student")
  }
}
struct Teacher {}
impl Hello for Teacher {
  fn say_something(&self) -> String {   // signature will be exactly the same
    String::from("I'm not a bad teacher")
  }

  fn say_hi(&self) -> String {
    String:from("Hi, I'm your new teacher") // we can still reimplement a default implementation
  }
}

fn main() {
    let s: Student = Student {};
    assert_eq!(s.say_hi(), "hi");
    assert_eq!(s.say_something(), "I'm a good student");

    let t: Teacher = Teacher {};
    assert_eq!(t.say_hi(), "Hi, I'm your new teacher");
    assert_eq!(t.say_something(), "I'm not a bad teacher");

    println!("Success!");
}
```

# Derive flag
The compiler is capable of providing basic implementations for some traits via the `#[derive]` attribute.
```rust
// `Centimeters`, a tuple struct that can be compared
#[derive(PartialEq, PartialOrd)]
struct Centimeter(f64)

// `Inches`, a tuple struct that can be printed ({:?})
#[derive(Debug)]
struct Inches(i32)

impl Inches {
    fn to_centimeters(&self) -> Centimeters {
        let &Inches(inches) = self; //&inches(12)

        Centimeters(inches as f64 * 2.54)
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
struct Seconds(i32);

fn main() {
    let _one_second = Seconds(1);

    println!("One second looks like: {:?}", _one_second);
    let _this_is_true = (_one_second == _one_second);
    let _this_is_true = (_one_second > _one_second);

    let foot = Inches(12);

    println!("One foot equals {:?}", foot);

    let meter = Centimeters(100.0);

    let cmp =
        if foot.to_centimeters() < meter {
            "smaller"
        } else {
            "bigger"
        };

    println!("One foot is {} than one meter.", cmp);
}
```

# Operators
operators are syntactic sugar for method calls. For example, the + operator in a + b calls the add method (as in a.add(b)).

This add method is part of the Add trait. Hence, the + operator can be used by any implementor of the Add trait.

In short: many of the operators can be overloaded via traits. That is, some operators can be used to accomplish different tasks based on their input arguments.
```rust
use std::ops;                                      // Look below as to why we are importing this library
                                                   // As mentiond above, `*` as `+` needs `T` to implement `std::ops::Mul` Trait.
fn multiply<T: std:ops:Mul<Output = T>>(a: T, b: T) -> T {    // the *trait bound* after `multiply` is to implement the trait `*`
  a * b                                            // only if both a and b implement the trait Mul, is possible to multiply them *
}             

fn main() {
    assert_eq!(6, multiply(2u8, 3u8));
    assert_eq!(5.0, multiply(1.0, 5.0));

    println!("Success!");
}
```
The `a * b` above is actually just synctatic sugar for `a.mul(b)`.

Why we have to specify an output? because Mul is defined as this:
```rust
pub trait Mul<Rhs = Self> {     // Rhs stands for Right Hand Side
  type output;                  // <---------

  // Required method
  fn mul(self, rhs: Rhs) -> Self::Output;
}
```
To better understand this above let's look at the following examples:
```rust
use std::ops;

struct Foo;
struct Bar;

struct FooBar;

// The `std::ops::Add` trait is used to specify the functionality of `+`.
// Here, we make `Add<Bar>` - the trait for addition with a RHS of type `Bar`.
// The following block implements the operation: Foo + Bar = FooBar
impl ops::Add<Bar> for Foo {
    type Output = FooBar;

    fn add(self, _rhs: Bar) -> FooBar {
        FooBar
    }
}
```
If we were to implement the trait `Add` only for `Foo` we could avoid to put anything next to `ops::Add`, because it defaults to `Self`
```rust
// Foo + Foo -> Foo.add(Foo)

impl ops::Add for Foo {
    type Output = Foo;

    fn add(self, _rhs: Foo) -> Foo {
        Foo
    }
}

// is same as \/

impl ops::Add<Self> for Foo {
    type Output = Foo;

    fn add(self, _rhs: Foo) -> Foo {
        Foo
    }
}
```
But we are trying to implement a trait with RHS `Bar`, that outputs a `FooBar` type.
```rust
// Foo + Bar -> Foo.add(Bar)

impl ops::Add<Bar> for Foo {
    type Output = FooBar;

    fn add(self, _rhs: Bar) -> FooBar {
        FooBar
    }
}
```

# Traits as Function Parameters
Instead of a concrete type for the item parameter, we specify the impl keyword and the trait name.

This parameter now would accept any type that implements the specified trait. 
```rust
trait Summary {
    fn summarize(&self) -> String;
}

#[derive(Debug)]
struct Post {
    title: String,
    author: String,
    content: String,
}

impl Summary for Post {
    fn summarize(&self) -> String {
        format!("The author of post {} is {}", self.title, self.author)
    }
}

#[derive(Debug)]
struct Weibo {
    username: String,
    content: String,
}

impl Summary for Weibo {
    fn summarize(&self) -> String {
        format!("{} published a weibo {}", self.username, self.content)
    }
}

fn main() {
    let post = Post {
        title: "Popular Rust".to_string(),
        author: "Sunface".to_string(),
        content: "Rust is awesome!".to_string(),
    };
    let weibo = Weibo {
        username: "sunface".to_string(),
        content: "Weibo seems to be worse than Tweet".to_string(),
    };

    summary(&post);  //should be a reference (&) not to be used and be available below.
    summary(&weibo);

    println!("{:?}", post);
    println!("{:?}", weibo);
}

// fn summary needs to work with two different types as arguments.
// What do those types share in common?
// They share the Summary trait. We impl both for Post and Weibo.
// we can tell fn that the type must implement the Summary trait.

fn summary(a: &impl Summary) {
    let output: String = a.summarize() // this will beahave differently based on Summary impl above.
    println!("{}", output)
}
```
Notice that `impl` has `&impl` ins `fn summary`, that's because we have to use `post` and `weibo` in the `println!` after those were used for `summary()`. So they get consumed, unless we use a reference as we did.

Another way to write the above is like this:
```rust
fn summary<T: Summary>(a: &T) {
    let output: String = a.summarize() // this will beahave differently based on Summary impl above.
    println!("{}", output)
}
```

# Return Types that Implement Traits
We can also use the `.. -> impl Trait` syntax in the return position to return a value of some type that implements a trait.

However, you can only use impl Trait if you’re returning a single type, use Trait Objects instead when you really need to return several types.

# Traits OBJECTS `&dyn`
`impl Trait` doesn't work when returning multiple types. Different implementations may be using different amounts of memory, but size of types must be known at compile time.

Trait Objects is essentially a pointer to any type that implements that given type.
```rust
trait Animal {}

struct Dog;
struct Cat;

impl Animal for Dog{}
impl Animal for Cat{}

// The trait object is behind a pointer, pointers are aways knowns in size at compile time
// It's always usize, the size of a pointer.
// &dyn is the prefix of an Object Trait type.
fn return_animal(s: &str) -> &dyn Animal {   
  match s {
    "Dog" => &Dog {},
    "Cat" => &Cat {},
    _ => panic!(),
  }
}

fn main() {
  let animal1 = return_animal("Cat");
  let animal2 = return_animal("Dog");
}

```
Flexible because the exact return type doesn't have to be known at compile time as long as the size is fixed.

It won't matter if Dog is bigger than Cat, because their pointer will always be the same size.

`dyn` stands for Dynamic, let's what that means below. \/

# Static vs Dynamic Dispatch
To really understand what's going on with Trait Objects we need first to understand this topic.

### Static
When we run this code in the compiler:
```rust
trait Animal {
  fn say_hi(&Self);
}

struct Dog;
struct Cat;

impl Animal for Dog {
  fn say_hi(&Self) {
    println!("Woof");
  }
}

impl Animal for Cat {
  fn say_hi(&Self) {
    println!("Meow");
  }
}

fn main() {
  let dog = Dog;
  let cat = Cat;

  dog.say_hi();
  cat.say_hi();
}
```
The compiler generates methods for each concrete type (Dog, Cat) 
```rust
struct Dog;
struct Cat;

impl Dog {
  fn say_hi(&Self) {
    println!("Woof");
  }
}

impl Cat {
  fn say_hi(&Self) {
    println!("Meow");
  }
}

fn main() {
  let dog = Dog;
  let cat = Cat;

  dog.say_hi();
  cat.say_hi();
}
```
In Static Dispatch the methods (Meow or Woof?) can be resolved at compile time, because it is known at compile time.

### Dynamic
Specific method to call is determined at runtime.

It creates a reference to a Trait Object using `&dyn` or `Box<dyn>`.

Compiler will build a `vtable` for that trait. Which contains a pointer to the `impl` of each method in the `trait` for the specific type of the object the reference points to.

Compiler will lookup in the vtable to determine which method should be called for which type that implements the given trait.

It causes overhead but it is more flexible.

With `Box<T>` we are able to allocate a type to the heap memory. For instance we can allocate a `i32` to the heap (which usually isn't) and get back a pointer.

When we use Box for a Trait Object `Box<dyn T>` it means that the type that `impl`ements that particular trait is going to be allocated to hte heap memory and we get back a pointer. But we will also get back a **V-Pointer** vptr.

The vptr (V-Pointer) points to a **V-Table**. The vtable has different fields, one of them a methods field, where all the methods from the concrete type are located. At runtime the compiler will check the vtable and find which methods were allocated for the type in the heap memory.
```rust
trait Animal {
  fn noise(&self);
}

struct Cat;
struct Dog;

impl Animal for Cat {
  fn noise(&self) {
    println!("Meow");
  }
}

impl Animal for Dog {
  fn noise(&self) {
    println!("Woof");
  }
}

fn random_animal(random_number: u8) -> Box<dyn Animal> {
  if random_number < 10 {
    Box::new(Cat {})
  } else {
    Box::new(Dog {})
  }
}

fn main() {
  let random_number = 5; // fake is random
  let animal = random_animal(random_number);
  animal.noise();
}
```

The compiler cannot know if the Cat or the Dog will be picked, therefore it doesn't know the size of the return value.

That's why we put it behing a `Box<dyn Animal>` (trait object) so now the size is known, because it will always be a pointer, which size is known.

But now let's look at the last row `animal.noise()`, the compiler really won't know if to call the noise implementation for Dog or the one for Cat.

That's exaclty why we use a `dyn`amic dispatch, wiht it the method to be used will be decided at run time.

The compiler will look at the vptr and then lookup in te vtable to look for the method related to the type selected. So let's say that we are at run time and random_animal returns a Cat, the compiler will see that the type in the heap memory the pointer points to is a Cat, so it will lookup for the methods that were implemented for the Cat type.. "Meow"

That's why we use `dyn`amic dispatch and why the static dispatch wouldn't work.

# `Box`
Pointer that allows to store data on the heap rather than the stack.

To use when you have a type whose size can not be known at compile time.

Returns a pointer to the data stored on the heap.

What are the differences with `&` reference?:
1. Box allocates memory on the heap and owns it. A reference only points doesn't own.
2. Box can be passed across scopes, reference has limited lifetime.
3. Box can be cloned reference can't.
4. Box can be used for pattern matching.

> We are trying to understand when to use `&dyn Type` Vs `Box<dyn Type>`.

It is all about letting methods that can't know what type they are receiving to be applied to pointers so that the compiler can check on the methods at runtime.

# Trait Bound
The `impl Trait` syntax works for straightforward cases but is actually syntax sugar for a longer form, which is called a trait bound.
```rust
fn sum<T: std::ops::Add<Output = T>>(x: T, y: T) -> T {}
```
```rust
impl<T: std::fmt::Debug + PartialOrd> Pair<T> {
```

# Associated Types
Allows to specify a type that is associated with the trait.

When `impl` we have to specify a concrete type, 

Associated types is a type placeholder that the trait method can use in their signature.

Similat to generic types, but more flexible because they allow a trait to have different *associated types* for different implementing types.

example:
```rust
trait MyTrait {
  type MyType;   // associated type

  fn get_my_type(&self) -> Self::MyType;
}
```
```rust
struct MyStruct {}

impl MyTrait for MyStruct {
  type MyType = i32;

  fn get_my_tape(&self) -> Self::MyType {
    return 42;
  }
}
```

# Lifetimes
just a way to make sure that the lifetimes of the scopes of variable are what we expect inside a function. Is a way to abstract away those to help compiler understand what we want and to constrain out intention.

Another way to think of it is that a lifetime `'a` really represents a locatio in the memory, and with that we make sure that a parameter netering the function that used memory `'a` is still the same memory that will be output. Value may be different, but we make sure that we are using memory that is valid and present.

```rust
fn lognest<'a>(s1: &'a str, s2: &'a str) -> &'a str {}

fn main() {
  let x: &'x str = "hi";
  let y: &'y str = "hello";
  let z: &'z str = "hey";

  let l1: &'l1 str = longest(x, y);
  let l2: &'l2 str = longest(l1, z);
}
```

```md
'x |"hi"    |  |          |
'y |"hello" |  | -> 'l1   |
'z |"hey"   |             |-> 'l2
```

# Concepts

## Ownership and Mutability are distinct concepts

```rust
fn main() {
    let data = "Rust is great!".to_string();

    string_uppercase(data);
}

// Should take ownership
fn string_uppercase(mut data: String) {
    data = data.to_uppercase();

    println!("{}", data);
}
```

In here we declare a variable as **im**mutable but then we can pass it to a function that treats it as mutable, and that works because the ownership is passed to that function.

In Rust, immutability and mutability refer to the ability to modify the contents of a variable, while ownership refers to the control over the memory associated with a value. When you pass ownership of a variable to a function, you are transferring control of that memory to the function.

The `string_uppercase` function takes ownership of the `String` and make it capitalised. However, the immutability or mutability of the original `greeting` variable in the `main` function is not affected by passing ownership. <ins>The original `greeting` variable is immutable, and that immutability is not changed by the function. What changes is ownership.</ins>

When you transfer ownership to a function, you lose access to the original variable in the calling scope, regardless of whether the variable was initially defined as mutable or immutable.