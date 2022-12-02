---
sidebar_position: 7
---

# Components
Components are one of the main parts of Scurry that make it unique.

Components are similar to objects in other languages (they are the "object" in
"object-oriented programming").

Much like other languages, they allow state to be grouped together combined with
behavior.

Instances are modeled after Python classes, so users familiar with that should
feel right at home!

## Declaring a Component
In order to make a component use the `decl` keyword.

```
decl MyComponent {}

// To make an instance, use function call syntax.
instance = MyComponent();
```
For now, instance can do almost nothing. However, it can be compared with other
instances out of the gate!

```
println(instance == MyComponent());
```
This program prints `True`.

## Fields
In order to allow state add state to this object, Scurry has fields.
```
decl MyComponent {
   field1,
   field2
}
```
Now, when an instance of MyComponent is made, it will be initialized with two
fields: `field1` and `field2`. These fields default to `Nil`.

If you try assigning to a field that hasn't been declared yet, an error is
thrown.

## Methods
```
instance = MyComponent();
println(instance.field1);
```
This actually fails! Fields cannot be directly read or written so a method needs
to be added in order to actually use these fields.

```
decl MyComponent {
   field1,
   field2,

   exp fn get_field1(self) {
      return self.field1;
   }
}
```
`exp` here denotes a public method. Methods are private by default.

```
instance = MyComponent();
println(instance.get_field1());
```
This program works, and prints `Nil`.

`MyComponent` is still pretty useless. In order to assign values to fields when
an instance of `MyComponent` is created, use the `$new` method.

```
decl MyComponent {
   field1,
   field2,

   fn $new(self, field) {
      self.field1 = field;
      self.field2 = 5;
   }

   exp fn get_field(self) {
      return self.field1 + self.field2;
   }
}

instance = MyComponent(33);
println(instance.get_field());
```
Now this actually has some functionality!

## Extra
With this knowledge, you should be able to write nice object-oriented code!
However, one of Scurry's distinguishing features is it's emphasis on component
design.

<!--- TODO: Link here -->
On that note, a more advanced look at components is available here.
