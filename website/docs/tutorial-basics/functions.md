---
sidebar_position: 5
---

# Functions
Functions in Scurry are similar to other languages, but have some features that make dynamic typing not as frustrating.

## The Basics
A function can be declared like this:
```
fn add(x, y) {
   return x + y;
}
```

Functions can be called with the call syntax, `()`.

```
println(add(1, 2));
```
As you might expect, this prints `3`.

## Typing
While this model *works*, if someone passed in **anything** of the wrong type, **any**
given function might crash with an unhelpful error message. This is downside
shared throughout all dynamically typed languages.

To make this less annoying, Scurry supports type annotations.

```
fn add(x: Int, y: Int) > Int {
   return x + y;
}
```
This function now clearly accepts two integers, and returns an `Int`.

### Type Checks
Additionally, Scurry allows for type checks **at runtime**, which is very
helpful for demystifying error messages.

Simply add an `!` to any function call and the types of the supplied arguments
will be checked when the function is called.

```
fn add(x: Int, y: Int) {
   return x + y;
}

add("String", 22)!;
```
Give this program a run! Instead of throwing an error at `x + y`, Scurry instead
tells you exactly what types were mismatched!

By using type annotations and type checks frequently in your functions, you can
create relatively type-safe programs!
