---
sidebar_position: 1
---

# Overview

A quick overview of the Scurry language.

## Installation

Scurry can currently be installed in **two** ways. More installation methods might
be supported in the future!

### Cargo
You need to have the Rust toolchain installed to use this method.
```bash
cargo install scurry
```

### Manual
You need to have the Rust toolchain installed to use this method.
```bash
git clone https://github.com/dzfrias/scurry
cd scurry
cargo build --release
```

## Getting Started
Create a file named `hello_world.scy` to begin your journey! Enter in:
```
println("Hello, World!");
```
Now run `scurry hello_world.scy` to see your message printed to the screen!

You can also start up the Scurry REPL, just enter `scurry` into your shell.
```
>> println("This is the REPL!");
This is the REPL!
Nil

>>
```
The `println` function returns `Nil`. We can get into those details later.

Scurry aims to have good compiler feedback. Mess around with the function call
syntax and see what Scurry tells you!

## Documentation Layout
The documentation is split up into three sections. The basics, builtin features,
and a more nuanced look into components.

### The Basics
[The basics](./category/the-basics) include:
- Syntax
- Scurry's builtin types
- Basic components
- Best practices

### Builtins
The builtin features docs include:
- All of Scurry's builtin functions
- The Scurry standard library

### Components
Finally, the components section explains:
- All component syntax
- Component state
- Best practices with components
