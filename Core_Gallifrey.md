This document describes the version of core Gallifrey that is implemented by this interpreter. Syntactically it has some similarities to IMP, extended with classes and functions. Arithmetic operations and control flow (if/while) are supported  but are omitted from this document for simplicity. 

The version of the type system that I implemented is closest to what the type system looked like in October 2019, with some major modifications (making variables immutable, adding an a validity bit for capabilities).

### Literals:
- integers or booleans (`true` or `false`)
- no object/record literals

### Declaration:
- `let x = e1 in e2`
- the type for x does not need to be annotated, and a fresh capability will be generated
- shadowing is only allowed inside the body of a function

### Assignment:
- `e1 = e2`
- variables are not mutable anymore, so the LHS of assignments must be a field

The copying/referencing semantics are similar to Java:
- if the RHS is a pointer to a mutable object, then the LHS pointer will point to the same object
- if the RHS is immutable, then the value will be copied to the location pointed at by the LHS

### Destroy:
- `destroy(e)` destroys (invalidates) the write capability of the value resulting from evaluating `e`

### Capof:
- `capof(e)` evaluates to the write capability of the value resulting from evaluating `e` (a variable or field) without consuming it
- result is `T_cap`; values of type `T_cap` cannot be stored
- used for passing in capability arguments to functions and for debugging

### Sleep
- `sleep(n)` sleeps for `n` seconds, where `n` is an integer literal

### Branch
- `branch x1, x2 ... xn { e }` creates a new branch (thread) and evalutes `e`
- the new evaluation context has the capabilities for the variables `x1` ... `xn`
- those capabilities are lost in the original context
- the value resulting from evaluating `e` is lost, and there is currently no way to recover the capabilities that are given to the branch

### Functions
- Core Gallifrey is functional; functions are anonymous but they can be bound to names
- Function syntax is a bit odd; it looks roughly like `fun ( Λ | λ ) -> t { e }`
- `t` is the return type, `e` is the body of the function
- the parentheses represent groups of arguments; Λ is a big lambda param and λ is a small lambda param; each group of arguments has up to 1 Λ and any number of λ, with a pipe symbol separating the Λ and λ params
- there is no partial application or optional/default arguments, but it is possible to make a function return another function. The number of arguments is exactly equal to the number of lambdas
- the closure doesn't close over the memory locations and values; when a function is applied the environment used is the context at the time of application. function application is only legal if the closed over store's variables all still have the same types and capabilities in the application context
- the output type of the function is only checked after it is applied and finishes evaluating (this is specific to this interpreter, not the type system)

#### Function Params
There are 3 types of function parameters. The capability annotations are not actual capabilities; they are meta-capabilities. The purpose of the Λ parameters is to take capabilities on function application, which it uses to verify the capabilities of the λ arguments.
- sigma-Λ params are written as `x: c t`. When the function is called it must be passed an object that is focused, and that object must be of type `t`
- λ params are written as `x : c t`. The meta-cap `c` must correspond to one that was previously defined in a Λ parameter. When the function is called it must be passed a value that has type `t` and write capability equal to the real capability that was passed in for `c`

#### Function Syntax and Meta-capabilities
The groupings of params don't really matter; they are parsed into a single list of lambdas, so there are multiple ways to write the same function signature. for example, `(c0 | a : c0 int)` is the same as `(c0 |) (|a : c0 int)`. However, the meta-capabilies must be alpha-equivalent so `(c0|) (c1 | a : c1 int)` is not the same as `(c0|) (c1 | a : c0 int)`. 

Also note that there's no restriction on which meta-caps you can use in a λ as long as it has been defined before. For example, `(c0 | a : c0 int) (c1 | b : c0 int)` is perfectly legal.

The syntax for writing a function type is exactly the same as declaring params; this means that the type also has param names, but those are not checked. thus, the types `(c0 | a : c0 int) -> int` and `(c0 | b : c0 int) -> int ` are the same. However, as above, the meta-capabilies must be alpha-equivalent.

As stated previously, the capability written in function definitions/signatures are actually meta-capabilities, not actual capability values. What this means is that when a Λ argument is provided, the meta-cap `c` is replaced with the actual capability (for sigma-Λ it is the capability of that focused object, for kappa-Λ it is the capability that was passed in). This replacement occurs in the subsequent λ's, unless there is a Λ that defines it.

For example, given a function f with the signature `(c0 | a : c0 int, b : c0 int) (c1 | c : c1 int) (c0 | d : c0 int) -> ...`, it would be applied as follows (assuming we have variables x, y, z declared as integers, with capabilities c.x, c.y, and c.z, respectively): `f(capof(x), x, x, capof(y), y, capof(z), z)`. The capabilities that are checked for in arguments a, b, c, and d are c.x, c.x, c.y, and c.z, respectively.

### Objects/Classes
- classes are declared with `class <name> { <field definitions> }`
- classes can optionally extend another class by using the syntax `class <name> extends <name> { ... }`. This inherits the other class's field definitions.
- 2 fields cannot have the same name, and overloading/shadowing a field name from a parent class is not allowed.
- the basic syntax for field definitions is `<name> : <type>`
- optionally, the `mut` and/or `U` keywords can be used before the field name, to mark the field as mutable or unique; they default to immutable/aliasable otherwise.
- field access is done with `.` synax

#### Constructors
Objects cannot be uninitialized, and must be created using constructors. Constructors are functions which are automatically generated based on the class definition and bound to the name of the class.

They have the following signature (taking into account the fields inherited from parent classes):
- 1 kappa-Λ for all aliasable fields (including the ones inherited from a parent class)
- 1 kappa-Λ for each unique field (in order of declaration)
- 1 λ for each field, in order of declaration

```
class C {mut a : int}; 
class D extends C {mut b : int, mut U c: int, mut d : int}; 
```

In the above example, C's constructor would have a signature `(c0 | a: c0 int) -> C`.
and D's constructor would have a signature `(c0 |) (c1 |) (| a: c0 int) (| b: c0 int) (| c: c1 int) (| d: c0 int) -> D`.

### Focus

TODO