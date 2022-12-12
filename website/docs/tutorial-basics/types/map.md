---
sidebar_position: 6
---

# Map
The builtin Map type in Scurry.

## Behavior
The `Map` type is similar to a hashmap or dictionary in other languages. 

- Does **not** remember the insertion order of items
- Keys can be [strings](String), [integers](Int), or [booleans](Bool)
- O(1) lookup times
- Can be indexed
  ```
  >> {"key": "value", 44: 99}[44];
  99
  ```
- Can be looped through
  ```
  >> for key in {1: 4, "a": True} { println(key); }
  1
  a
  ```
- False when empty

## Methods

### keys()
Returns an [Array](Array) of the keys of the map. Note that these will be
returned in no particular order.

```
>> {True: 10.3, "key": [1, 2, 3]}.keys();
[True, "key"]
```

### values()
Returns an [Array](Array) of the values of the map. Note that these will be
returned in no particular order.

```
>> {True: 10.3, "key": [1, 2, 3]}.values();
[10.3, [1, 2, 3]]
```

### remove(item: Any)
Removes an item from the map.

```
>> x = {3: 3};

>> x.remove(3);
Nil

>> x;
{}
```

### merge(map: Map)
Merges the map with another.

```
>> x = {1: 3};

>> x.extend({3: 1});

>> x;
{1: 3, 3: 1}
```
