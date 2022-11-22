---
sidebar_position: 2
---

# If Statements
Conditional logic in Scurry is quite simple, and users familiar with Python's
truthy and falsey values should pick up Scurry's in no time.

## Use
```
if 3 * 3 > 8 {
   println("True!");
}
```
This small program will print out `True!`, as you might have expected.

Indentation doesn't matter but **4 spaces** are the convention in Scurry. Any
time a block is surrounded in curly braces, it should be indented.

### Else
Like many other languages, Scurry includes an `else` clause.
```
if False {
   println("This should not appear");
} else {
   println("This should!");
}
```

### Else If
Scurry's variant of the classic `else if` or `elif` deviates just as much as
everything else so far on this page. That is to say, it is completely the same
as most languages.
```
if False {
   println("This should not appear");
} elif 4 != 4 || False {
   println("This also shouldn't appear");
} elif 5 == 5 {
   println("This should!");
}
```
Note the use of the logical or operator, `||`, in the first `elif` clause.

## Extra Note
In Scurry, everything can be coerced into a [Bool](./types/bool.md). For
example, the [Int](./types/int.md) is coerced to `False` if it `0`, but it 
is `True` otherwise.

This coercion **only** happens in `if` statements, and can be called explicitly
with the `truthy` function. <!--- TODO: Put links here -->

Here's a full list of what makes an something truthy or falsey:
- [Int](./types/int.md): `False` when `0`
- [Float](./types/float.md): `False` when `0.0`
<!--- TODO: Put links here -->
- `String`: `False` when empty
- `Array`: `False` when empty
- `Map`: `False` when empty
- Everything else is always `False`
