---
sidebar_position: 1
---

# Int

The integer implementation in Scurry. The `Int` type supports 32-bit signed
integers and all the normal arithmetic operations (`+`, `-`, etc.).

## Behavior
- Seamless interaction with the [Float](float) type
  ```
  >> 2.3 + 1;
  3.3
  ```
- Falsey when `0`
- Errors in the event of overflow
  ```
  >> 2147483647 + 1;
  integer overflow occured in the expression: `2147483647 + 1` on line 1
  ```
- Zero division errors
  ```
  >> 3 / 0;
  division by zero occured in the expression: `3 / 0` on line 1
  ```

## Methods
The `Int` type has the following methods.

### abs()
Converts the integer to it's absolute value.
```
>> (-1).abs();
1

>> 1.abs();
1
```

### to_float()
Casts the integer to the [Float](float) type.
```
>> 1.to_float();
1.0
```
