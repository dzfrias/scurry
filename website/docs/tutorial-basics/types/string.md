---
sidebar_position: 4
---

# String
The builtin `String` type in Scurry.

## Behavior
- Immutable/passed by value
- Can be concatenated with the `+` operator
  ```
  >> "Hello" + " world!";
  "Hello world!"
  ```
- Can be indexed
  ```
  >> "Test"[1];
  "e"
  ```
- Falsey when empty
- Supports unicode characters
  ```
  >> "中文"[0];
  "中"
  ```
- Can be looped through
 ```
  >> for char in "String" { println(char); }
  S
  t
  r
  i
  n
  g
  ```
- Quotes can be escaped with a `\`
  ```
  >> "this is valid: \"!!";
  "this is valid: \"!!"
  ```

## Methods
The `String` type has the following methods.

### len()
Returns an [Int](./int.md) containing the number of characters.

```
>> "test".len();
4
```

### trim()
Returns a new `String` with all whitespace on the left and right ends stripped
away.

```
>> " hello ".trim();
"hello"
```

### starts_with(s: String)
Returns `True` if the string starts with `s`, but `False` otherwise.

```
>> "does it start with?".starts_with("does");
True
```

### ends_with(s: String)
Returns `True` if the string ends with `s`, but `False` otherwise.

```
>> "doesn't end with".starts_with("what");
False
```
### substring(n1: Int, n2: Int)
Returns a new `String` that is within `n1` and `n2`. `n1` is inclusive, `n2` is
not.

```
>> "get a substring!".substring(0, 4);
"get"
```

### split(delim: String)
Return an [Array](array) consisting of the string split by `delim`.

```
>> "split into three".split(" ");
["split", "into", "three"]
```
