---
sidebar_position: 2
---

# Float

The float implementation in Scurry. The `Float` type supports 32-bit floating
point numbers and all the normal arithmetic operations (`+`, `-`, etc.).

## Behavior
- Seamless interaction with the [Int](int) type
  ```
  >> 2.3 + 1;
  3.3
  ```
- Falsey when `0.0`
- No NaN's or infinites

## Methods
The `Float` type has the following methods.

### abs()
Converts the float to it's absolute value.
```
>> (-1.0).abs();
1.0

>> 1.0.abs();
1.0
```

### to_int()
Casts the float to the [Int](int) type. This method will truncate every digit
after the `.`.
```
>> 1.9.to_int();
1
```
