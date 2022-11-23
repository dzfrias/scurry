---
sidebar_position: 3
---

# Loops
Loops in Scurry are similar to those in modern languages.

There are two types of loops:
- `While` loops
- `For` loops

## While Loop
`While` loops in Scurry have no surprises:
```
while 3 == 3 {
   println("This will run forever!");
}
```
`While` loops follow the same boolean logic as [if statements](./if_stmt).

## For Loop
Scurry uses the common `for ... in ...` loop style, similar to
[Python](https://www.python.org) or [Rust](https://www.rust-lang.org). Users
familiar with those languages should pick up Scurry's `for` loops in no time!

```
for i in Range(0, 3) {
   println(i);
}
```
<!--- TODO: Link here -->
`Range` is a builtin iterator, and iterators are what allows for the looping.
In this program, `0`, `1`, and `2` will be printed.

### Builtin Types
<!--- TODO: Link here -->
`Array`s can be looped through.
```
for item in [1, 2, 3] {
   println(item);
}
```

<!--- TODO: Link here -->
`String`s can be looped through.
```
for char in "Hello!" {
   println(char);
}
```
<!--- TODO: Link here -->
Scurry does not have a dedicated `char` type, so the `char` variable in this
example will be another `String`

<!--- TODO: Link here -->
`Map`s can be looped through.
```
for key in {"test": 0, 10: 0} {
   println(key);
}
```
`key` will be the keys of the map.

### Custom Iterators
<!--- TODO: Link here -->
You can define your own iterator using the `Iterator` component! More on that
here.
