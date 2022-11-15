# Scurry
![CI](https://github.com/dzfrias/scurry/actions/workflows/rust.yml/badge.svg)

Scurry is an dynamically typed component-based object-oriented language,
written in [Rust](https://github.com/rust-lang/rust). Scurry is still under
active development and thus more features will be available soon.

...

That was a mouthful. So what can it actually do?

## Overview
Here are some general features:
```
// Basic arithmetic
1 + 1; 2 * 2; 3 / 3; 4 - 4; 5 % 5;

// Control flow
for i in [1, 2, 3] {  // Dynamically sized arrays
    println(i);
}

// Loops include `break` and `continue`!
while True {
    println("This is infinite.");
}

if True {
    println("True");
} elif 3 != 3 {
    println("This makes no sense!!");
} else {
    println("False");
}

// Variable assignment too
x = 5555;
switch x {
    case 5 {
        println("Why would this be true");
    }

    default {
        println("The last resort");
    }
}

// First class functions with lexical scoping
fn this_cool_function(x) {
    return fn() { return x; };
}
println(this_cool_function(3)());

// Data structures
array = [1, 2, 3];
array.push("cool!");

map = {"key": 4, 99: True};
map[42] = "value";
```

These are pretty nice, but the main feature of this language are components.
Components designed are small reusable objects that can share behavior
without sharing state. As shown in this trivial example:
```
decl ThreeNumbers {
    // These are the possible fields of ThreeNumbers. Fields are always
    // private.
    field1
    field2
    field3

    // $ prefixes special methods, this one is called when an instance is made
    fn $new(self, field1, field2, field3) {
        self.field1 = field1;
        self.field2 = field2;
        self.field3 = field3;
    }

    // `exp` denotes public methods
    exp fn sum(self) {
        return self.field1 + self.field2 + self.field3;
    }
}

// New instance of ThreeNumbers
numbers = ThreeNumbers(50, 5, 500);
println(numbers.sum());

decl ThreeNumbersAndString {
    number1
    number2
    number3
    string

    // Calls ThreeNumbers's $new at the end of this component's $new, passing
    // these fields
    [ThreeNumbers] {
        number1
        number2
        number3
    }

    fn $new(self, n1, n2, n3, string) {
        self.number1 = n1;
        self.number2 = n2;
        self.number3 = n3;
        self.string = string;
    }

    exp fn get_string(self) {
        return self.string;
    }
}

// str_nums cannot access any state from ThreeNumbers! Only methods
str_nums = ThreeNumbersAndString(50, 5, 500, "hello!");
println(str_nums.sum());
println(str_nums.get_string());
```
This language encourages small components with very little state that can be
modified and small API surface areas.

## Installation
Currently, Scurry can be downloaded with the following methods.

### Cargo
Must have the Rust toolchain installed.

```
$ cargo install scurry
```

### Manual
Must have the Rust toolchain installed.

```zsh
$ git clone https://github.com/dzfrias/scurry
$ cd scurry
$ cargo build --release
$ # Now put the binary wherever you'd like!
```

## License
Scurry is licensed under the MIT License.
