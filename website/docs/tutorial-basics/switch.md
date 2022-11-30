---
sidebar_position: 4
---

# Switch Statements
Switch statements in Scurry allow for readable conditional code.

## Syntax
The syntax of switch statements differs from other languages that have something
similar.

In Scurry, a switch statement is as follows:
````
switch 44 {
   case 33 {
      println("33");
   }

   case 99 {
      println("99");
   }

   default {
      println("default")
   }
}
````
This code, as you might expect, outputs `default`.

Cases can have multiple conditions with a pipe (`|`).
```
switch "string" {
   case "s" | "string" {
      println("got it!")
   }
}
```

## Notes
- Switch statements are not O(1) in Scurry
- They do not have to be exhaustive
