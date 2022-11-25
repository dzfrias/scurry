---
sidebar_position: 5
---

# Array
The builtin `Array` type in Scurry.

## Behavior
- Can hold items of any type
  ```
  >> [1, 3.3, "string", Nil];
  [1, 3.3, "string", Nil]
  ```
- Can be indexed
  ```
  >> [0, 3][1];
  3
  ```
- Can be looped through
  ```
  >> for item in [1, 2, 3] { println(item); }
  1
  2
  3
  ```
- Mutable/passed by reference
- Falsey when empty

## Methods
The `Array` type has the following methods.

### len()
Returns an [Int](int) representing the number of items.

```
>> [4, 4, 4, 4].len();
4
```

### push(item: Any)
Appends `item` to the back of the array.

```
>> x = [1, 2, 3];

>> x.push(4);
Nil

>> x;
[1, 2, 3, 4]
```

### pop()
Removes the last item in the array, returning its value. `Nil` is returned if
the array is empty.

```
>> x = [1, 2, 3];

>> x.pop();
3

>> x;
[1, 2]
```

### contains(item: Any)
Returns `True` if `item` is in the array, and `False` otherwise.

```
>> [1, 2, 3].contains(4);
False
```

### insert(item: Any, index: Int)
Inserts `item` into the index provided. `index` must be within the bounds of the
array.

```
>> x = [1, 2, 3];

>> x.insert(0, 0);
Nil

>> x;
[0, 1, 2, 3]
```

### remove(index: Int)
Removes the item at the given index.

```
>> x = [1, 2, 3];

>> x.remove(1);
Nil

>> x;
[1, 3]
```

### concat(array: Array)
Returns a new array that combines the `self` and `array`.

```
>> [1, 2, 3].concat([4, 5, 6]);
[1, 2, 3, 4, 5, 6];
```
