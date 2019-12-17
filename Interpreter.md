### Design Goals:

The goal of this interpreter is to model the semantics of Gallifrey, and to verify that well-typed/well-formed Gallifrey programs have the desired properties at runtime (and that programs which are not well-typed violate those properties). 

The interpreter contains a lot of runtime checks/asserts  at each step of evaluation, so that if an invariant is violated or unexpected behavior occurs there will be a runtime exception. This helps us make sure that programs we expect to evaluate successfully and correctly will do so, and programs we expect to fail will cause a runtime exception.

The runtime checks are designed to be easy to remove; most of them are in the form of assert statements (`g_assert`) or factored out into separate functions.

Runtime checks include: values are the correct types, `K'` and `P` do not overlap, capabilities can't be both valid and invalid at the same time, the appropriate capabilities exist and are valid when reading and writing, etc.

### Runtime State
At runtime the interpreter keeps track of the following things:
- K: list of available capabilities (and whether they are valid)
- Gamma: store mapping variable names and types to locations (implemented as a stack of hash tables for purposes of scoping)
- Sigma: focus stack (functional stack of capability, type, location triplets)
- C: a list of available class names and their field layouts
- Memory: an abstraction for memory locations, implemented as an Int->Value hash table
- in_func: a flag for whether or not the evaluation context is inside a function body (for purposes of banning or allowing shadowing)
- counter: for generating fresh integers to use for memory locations and capability names

There is a double-indirection system with pointers, such that the location for a variable in the store (and the location for a field stored in an object value) is the memory location for the pointer to that value. Thus, having `x = 5` and `y = C(10)` where the definition of C is `class C {mut a : int}` would have a store and memory looking like this:

```
store: [x -> 0, y -> 2]

memory: [
    0 -> V_ptr(1), 
    1 -> V_int(5), 
    2 -> V_ptr(3), 
    3 -> V_obj(C, [a -> 4]), 
    4 -> V_ptr(5), 
    5 -> V_int(10)
]
```
Assigning `y.a = x` would result in the value stored at location 4 to be changed to `V_ptr(1)`.

### Testing:

There is an automated test suite for the interpreter, with tests split into 3 types: 
- value (check the returned value)
- success (check that program succeeded evaluating)
- failure (check that program failed at runtime)

Tests are defined with a string of source code that should be run; additionally an expected value must be provided for the value tests. Lexing or parsing errors in any of the tests will cause the test suite itself to fail, and will not be counted as a runtime exception. 

Tests which check for failure only check that an exception occurred, not what type of exception, so it is recommended to pair them with a similar test that will not throw an exception at runtime.

### Known Bugs

Precedence for parsing is not that great, and in general the parser does not fail in all the cases that it should. For example, there are limitations for what can be on the left hand side of an assignment and what can be used for destroy/sleep, but those are checked at runtime by the interpreter.

Example parsing issue: the string `x.w(x) + x.w(x)` gets parsed as `(x.w(x) + x.w)(x)` instead of `(x.w(x)) + (x.w(x))`
