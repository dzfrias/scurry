---
sidebar_position: 6
---

# Import Statements
Like any standard programming language, Scurry supports modularity.

## Basics
The `import` statement is nothing magical!

*In hello.scy*
```
exp fn hello() {
   println("hello");
}
```

*In another file in the same directory*
```
import "hello";

hello.hello();
```
This program prints `hello` to the screen!

The `import` statement accepts a string after it. The string can be an absolute
path or a relative one. Note that the `.scy` file extension doese **not** have
to be specified.

Unlike other languages, you cannot import specific parts, and must import the
entire file.

## As Keyword
In order to import a file under a different, name Scurry has the `as` keyword.

```
import "some_file" as new_name;

// ...
```
This will bring `new_name` into scope instead of `some_file`.
