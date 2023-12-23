```rust
type User {
  LoggedIn { count: Int }  // A logged in user
  Guest                    // A guest user with no details
}

fn try(x:Int) -> List<User> {
  let user = LoggedIn {count: x}
  let visitor = Guest

  [user, visitor]
}
```

```rust
type User {
  LoggedIn { count: Int }  // A logged in user
  Guest                    // A guest user with no details
}

fn try(x:Int) -> User {
  let user = LoggedIn {count: x}

  user
}

test foo() {
  try(3) == LoggedIn(3)
}
```

## Destructuring

```rust
fn get_head(a: List<a>) -> Option<a> {
  when a is {
    [a, ..] -> Some(a)
    [] -> None
  }
}

fn get_name(user) { 
  when user is {
    LoggedIn { count } -> count
    Guest -> "Guest user"
  }
}
```

Custom types can also be destructured with a let binding.

```rust
type Score {
  Points(Int)
}
 
let score = Points(50)
let Points(p) = score // This brings a let-binding `p` in scope.
 
p // 50
```

During destructuring you may also use discards (_) or spreads (..).

```rust
type Dog {
  Dog { name: ByteArray, cuteness: Int, age: Int }
}
 
let dog = Dog { name: #"436173686577", cuteness: 9001, age: 3 }

// All fields present
let Dog { name: name, cuteness: _, age: _ } = dog
builtin.decode_utf8(name) // "Cashew"
 
// Other fields ignored by spreading.
// Field punning is supported. Hence `age` is a shorthand for `age: age`.
let Dog { age, .. } = dog
age // 3
```

## Named Accessor

```Rust
let dog = Dog { name: #[82, 105, 110], cuteness: 2001 }
dog.cuteness // This returns 2001
```

## Generics

```Rust
type Box<inner_type> {
  Box(inner: inner_type)
}

fn foo() {
  let a = Box(420) // type is Box<Int>
  let b = Box("That's my ninja way!") // type is Box<String>
}
```

## Record Updates

```rust
type Person {
  name: ByteArray,
  shoe_size: Int,
  age: Int,
  is_happy: Bool,
}
 
fn have_birthday(person) {
  // It's this person's birthday, so increment their age and
  // make them happy
  Person { ..person, age: person.age + 1, is_happy: True }
}
```

## Type Aliases

```rust
type MyNumber = Integer
```

```rust
type Person = (String, Integer)
 
fn create_person(name: String, age: Integer) -> Person {
  (name, age)
}
```

## Data (Casting)

Any Custom Data in Aiken is then dealt with as Data, an opaque data representation. If you want to go from Data to Custom Data you need to use `expect`

```rust
fn to_datum(datum: Data) -> Datum {
    expect d: Datum = datum
    d
}
```

```rust
type MyDatum {
  foo: Int,
  bar: ByteArray,
}
 
fn to_my_datum(data: Data) -> MyDatum {
  expect my_datum: MyDatum = data
  my_datum
}
```

## Control Flow

Aiken's `when *expr* is` is an expression, meaning it returns a value and can be used anywhere we would use a value.

```rust
when some_number is {
  0 -> "Zero"
  1 -> "One"
  2 -> "Two"
  n -> "Some other number" // This matches anything
}
```

```rust
type Answer {
  Yes
  No
}
 
let answer = Yes
 
let description =
  when answer is {
    Yes -> "It's true!"
    No -> "It's not yes."
  }
 
description == "It's true!"
```

`if/else`

```rust
fn fibonnaci(n: Int) -> Int {
  if n == 0 {
    0
  } else if n == 1 {
    1
  } else {
    fibonnaci(n-2) + fibonnaci(n-1)
  }
}
```

[control flow continues ](https://aiken-lang.org/language-tour/control-flow)


## Modules

```rust
// inside module lib/straw_hats/sunny.ak
 
fn count_down() {
  "3... 2... 1..."
}
 
fn blast_off() {
  "BOOM!"
}
 
pub fn set_sail() {
  [
    count_down(),
    blast_off(),
  ]
```

The `pub` keyword makes this type usable from other modules. Otherwise funcs would be private.

Functions, type-aliases and constants can all be exported from a module using the `pub` keyword.

```rust
// inside module src/straw_hats/laugh_tale.ak
 
use straw_hats/sunny
 
pub fn find_the_one_piece() {
  sunny.set_sail()
}
```

```rust
use unix/dog
use animal/dog as kitty
```

```rust
use animal/dog.{Dog, stroke}
 
pub fn foo() {
  let puppy = Dog { name: "Zeus" }
  stroke(puppy)
}
```

This may be useful for values that are used frequently in a module, but generally qualified imports are preferred as it makes it clearer where the value is defined.

## Opaque Types

At times it may be useful to create a type and make the constructors and fields private so that users of this type can only use the type through publicly exported functions.

For example we can create a Counter type which holds an int which can be incremented. We don't want the user to alter the Int value other than by incrementing it, so we can make the type opaque to prevent them from being able to do this.

```rust
// The type is defined with the opaque keyword
pub opaque type Counter {
  Counter(value: Int)
}
 
pub fn new() {
  Counter(0)
}
 
pub fn increment(counter: Counter) {
  Counter(counter.value + 1)
}
```

Because the Counter type has been marked as opaque it is not possible for code in other modules to construct or pattern match on counter values or access the value field. Instead other modules have to manipulate the opaque type using the exported functions from the module, in this case new and increment.

## Tests

One exciting thing about tests is that they use the same virtual machine as the one for executing contracts on-chain. Said differently, they are actual snippets of on-chain code you can run and reason about in the same context as your production code.

```rust
test foo() {
  1 + 1 == 2
}
```

You can write tests anywhere in an Aiken module, and they can make calls to functions and use constants all the same. However, tests cannot call other tests! If you need to re-use code between tests, create a function.

the report group tests by module and gives you, for each test, the memory and CPU execution units needed for that test. That means tests can also be used as benchmarks if you need to experiment with different approaches and compare their execution costs.

### Tests Failures

Sometimes, you need to assert that a certain execution path can fail. This is also known as an "expected failure" and is a totally valid way of asserting the behavior of a program. Fortunately, you can do this with Aiken too by prefixing the test keyword with a bang !. So for example:

```rust
lib/example.ak
use aiken/math
 
!test must_fail() {
  expect Some(result) = math.sqrt(-42)
  result == -1
}
```

### Running specific tests
aiken check supports flags that allow you to run subsets of all tests in your project.

Examples

```
aiken check -m "aiken/list"
```

This only run tests inside of the module named aiken/list.
```
aiken check -m "aiken/option.{flatten}"
```

This only runs tests within the aiken/option module that contains the word flatten in their name.

```
aiken check -e -m "aiken/option.{flatten_1}"
```

You can force an exact match with -e.

```
aiken check -e -m map_1
```

This only run tests in the whole project that exactly match the name map_1.

# [Troubleshooting](https://aiken-lang.org/language-tour/troubleshooting)

## Traces

```rust
fn is_even(n: Int) -> Bool {
  trace "is_even"
  n % 2 == 0
}
 
fn is_odd(n: Int) -> Bool {
  trace "is_odd"
  n % 2 != 0
}
```

```rust
let n = 10
is_even(n) || is_odd(n)

```

Only the trace `is_even` will be captured, because `is_odd` is in fact never evaluated (there's no need because the left-hand side already returns `True`).

There are two common ways to capture traces in Aiken: when running tests via `aiken check` or when simulating a transaction using `aiken tx simulate`. In both cases, traces captured during evaluation will be printed on screen.

## `?` operator

This postfix operator can be appended to any boolean expression and will trace the expression only if it evaluates to False. This is useful to trace an entire evaluation path that led to a final expression being False. In the example above, we could have written:

```rust
must_be_after? && must_spend_token?
```

Which would have generated the trace "must_spend_token ? False".

## [CBOR](https://aiken-lang.org/language-tour/troubleshooting#cbor-diagnostic)

```rust
use aiken/cbor
 
 
// An Int becomes a CBOR int
cbor.diagnostic(42) == @"42"
 
// A ByteArray becomes a CBOR bytestring
cbor.diagnostic("foo") == @"h'666F6F'"
 
// A List becomes a CBOR array
cbor.diagnostic([1, 2, 3]) == @"[_ 1, 2, 3]"
 
// A Tuple becomes a CBOR array
cbor.diagnostic((1, 2)) == @"[_ 1, 2]"
 
// A List of 2-tuples becomes a CBOR map
cbor.diagnostic([(1, #"ff")]) == @"{ 1: h'FF' }"
 
// 'Some' is the first constructor of Option → tagged as 121
cbor.diagnostic(Some(42)) == @"121([_ 42])"
 
// 'None' is the second constructor of Option → tagged as 122
cbor.diagnostic(None) == @"122([])"
```

```rust
type MyDatum {
  foo: Int,
  bar: ByteArray
}
```

Eventually, you will need to construct compatible values for building an associated transaction. Aiken provides blueprints as build outputs to help with that. Yet you may also control some chosen values directly using `cbor.diagnostic` and a test:

```rust
use aiken/cbor
 
test my_datum_1() {
  let datum = MyDatum { foo: 42, bar: "Hello, World!" }
  cbor.diagnostic(datum) == @"121([42, h'48656c6c6f2c20576f726c6421'])"
}
```

# Used cmd and funcs

```
aiken new aiken-lang/hello_world
cd hello_world
```

## Examples 

```rust
fn trial(y: Int, b: List<a>) ->  List<a> {
  let xs = span(b, y-1)
  when xs is {
    (a, [_, ..bs]) -> concat(a,bs)
    _other -> b
  }
  // or_else(tail(slice(b, from: y, to: -1)), [])
}

test replaceAt_x() {
  let elem : List<Option<Player>> = [Some(O), None, Some(X)]
  trial(2, elem) == [Some(O), Some(X)]
}
```