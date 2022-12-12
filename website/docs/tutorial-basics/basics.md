---
sidebar_position: 1
---

# Introductory Syntax

This section quickly covers standard syntax in Scurry.

Semicolons are **not** optional. Scurry uses curly-brace syntax consistently
for just about everything in the language.

## Operators
Almost **every** standard operator, save maybe bitwise operators, makes an
appearance in Scurry. Just play around in the REPL and see what's possible!
```
>> 1 + 1;
2

>> True == False;
False

>> 5.3 >= 2;
True

>> True || False;
True
```

## Literals
- [Int](./types/int.md) and [Float](./types/float.md) literals, you can
  *probably* guess how to declare them
  - No hexadecimal literals
  - No binary literals
  - No `_` in numbers
- `String` literals, only double quotes allowed
   - `"This is a string!"`
- `True` for [boolean](./types/bool.md) true
- `False` for [boolean](./types/bool.md) false
- `Nil` for null
- Brackets for `Array` literals. Arrays are dynamically sized
   - `[1, 2, 3, Nil]`
- Curly braces for `Map` literals. `Map`s are similar to Python dictionaries.
   - `{"hello": 42, 10: 3.3}`

## Assignment
Variables are assigned with no more than an `=`.
```
>> pi = 3.14159265358979;

>> pi;
3.14159265358979

>> x = 3;

>> x /= 3;

>> x;
1

>> y;
variable not found: y on line 1
```
Again, play around in the REPL with everything learned. This stuff is pretty
self-explanatory but some more complicated elements of Scurry are best learned
through experimentation.
