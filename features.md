# Chirp features

This is an overview of the syntax and some features of the Chirp language.

See [examples](/examples) for example programs.

[statements]: #statements
[definition]: #defn-statement
[expression]: #expressions
[let]: #let-expressions
[if]: #if-expressions
[hint]: #hinted-expressions

A Chirp program is made up of any number of [statements] (including zero),
separated by line breaks or commas. Type [hint]s are _optional_ everywhere,
but may be necessary if there is not enough information to infer types.

Here is an example program:

```
defn putS(s: string): #() = foreign

putS("Hello, world!\n")
```

It is made up of two statements:

This [definition]:
```
defn putS(s: string): #() = foreign
```

This function call:
```
putS("Hello, world!\n")
```

# Statements

There are two kinds of statements in Chirp: [definition]s and [expression]s.

## Defn Statement

A `defn` statement introduces a binding in the scope that it is in.

```
defn x = 100
// define `x` as the value `100`

defn f(x) = x + 1
// define a function `f` that adds 1 to its input `x`

defn f(x: i64): i64 = x + 1
// same as above, but with type hints

defn id<T>(x: T): T = x
// define an identity function with an explicit generic type

defn putS(s: string): #() = foreign
// declare a foreign function putS
```

A `defn` statement can take multiple forms. Without a parameter list,
it just defines a local variable to a value. With a parameter list,
it defines a function.

### Mutual Recursion

Function bindings may be used in other functions _before_
the `defn` statement that defines them. Thus, two functions can
refer to each other, and call each other recursively:

```
defn isOdd(x) = x != 0 && isEven(x - 1)
defn isEven(x) = x == 0 || isOdd(x - 1)
```

Functions should never be called before their corresponding `defn`.
Note that this is currently not enforced by the compiler, and it merely causes
undefined behaviour at runtime.

### Foreign Function Interface

A `defn` statement may also declare a reference
to a _foreign_ function or variable using the `foreign` keyword.
Here, "foreign" refers to something written in a different language
and linked with the Chirp program when compiling. Functions written in C
(and any other functions with a C ABI) can be invoked this way.

## Expressions

### Binary Expressions

Chirp supports many common binary operators.

These are, in order of precedence:

#### Short-circuiting logical `AND` and `OR`
```
x && y || z
```
Boolean expressions are evaluated left to right,
evaluating the next one only if the result of the
previous expressions was:

- (in the case of `&&`) true
- (in the case of `||`) false

Unlike in other languages, these are left associative:
```
w && x || y && z 
```
is
```
((w && x) || y) && z
```
unlike C where it would be
```
(w && x) || (y && z)
```
#### Short-circuiting chained comparisons
```
t < u <= v > w >= x != y == z
```
Compare each corresponding pair of expressions,
returning early on the first that is false.

The example is equivalent to:
```
t < u && u <= v && v > w && w >= x && x != y && y == z
```
#### Bitwise `AND` and `OR`
```
x | y & z
```
Compute the bitwise `AND` or `OR` of the integer operands.
#### Addition and subtraction
```
x + y - z
```
Compute the sum or difference of the operands.
#### Multiplication, division and remainder
```
w * x / y % z 
```
Compute the product, quotient or remainder of the operands.
For integers, the quotient is rounded down.


### Hinted Expressions

Any expression may be followed with a type hint,
indicating that the type checker enforces a certain
type or certain trait boundaries for an expression.

```
(...): i64

(f: Fn<#(i64), #()>)(10)
```

A type hint can be one of:

- A type or trait name, optionally
  followed by type args in angled brackets
```
(...): i64
(...): Fn<#(i64), i64>
```
- A tuple, indicating an aggregate type
```
(...): #()
(...): #(i64, i64, i64)
```
- An explicit placeholder
```
(...): _
(...): Fn<_, i64>
```

### Prefix Expressions

Expressions may be prefixed with any number of
`+`s (doing nothing) or
`-`s (each negating the operand).

```
-(x + y + z)
+(x + y + z)
```

### Function Call Expressions

A function can be called by placing zero or more
arguments after it in brackets:

```
f(x)
g(x, y, z)
h()
```

[Tail call](https://en.wikipedia.org/wiki/Tail_call) optimisation (TCO) is _guaranteed_
when invoking the innermost function or named let from tail position.

For example:
```
defn foo(n) =
  let x = n, y = 0 in loop:
    if x < y {
      y
    } else if x == y {
      foo(y) // not guaranteed TCO, foo is not the innermost function
    } else {
      loop(x - 1, y + 1) // guaranteed TCO, loop is the innermost function
    }
```

Note that a line break cannot be included between the function and the argument list.

This:
```
f
(x)
```
would not be parsed as a function call, but as two separate expressions.

### Variable Expressions

Symbols given meanings with [definition]s or [let]s
may be referred to with just the symbol:

```
defn f(x) = x + 2
defn y = 10
f(y)
```

### Literal Expressions

String, boolean, integer and float literals can be written:

```
"Hello there!"
true
false
10
0.3
```

### Delimited Expressions

Expressions may be wrapped in a pair of parentheses.

```
(x + y)
```

A list of zero or more statements and a final expression
may be wrapped in a pair of curly braces, as a block expression.
Each statement must be followed by either a line break or a comma.

```
{
  defn x = 10, defn y = 20
  putS("Adding two numbers!")
  x + y
}
```

An empty pair of curly braces returns an empty tuple.

```
{}
```

Additionally, when it would _not_ be interpreted as a type hint,
a colon may prefix another expression:

```
: 10
```

This may be useful with [let] or [if] expressions:

```
if x <= 0 { 0 } else:
x + 2

let x = 10 in:
x / 2
```

#### If Expressions

An if expression checks whether its predicate is true,
returns the value of the `then` expression if it is, and the
`else` expression otherwise, if present. Additionally,
`else if` may be used to chain multiple checks.

```
if x == 10 {
  "ten"
} else if x == 20 {
  "twenty"
} else {
  "something else"
}
```

#### Let Expressions

A let expression may be used to introduce named bindings locally.

```
let x = 10, y = 20 in: x + y
```

Bindings follow the same format as [definition]s, and must be separated
by either a line break or a comma.

A let expression may be given a name after the `in` token:

```
let x = 10 in loop: ...
```

The name is then bound to a function that invokes the body of the let expression again,
replacing the local bindings introduced by the let expression with the arguments
of the function. As such, it may be used for looping.

```
let x = n in loop:
  if x == 1 {
    true
  } else if x % 2 == 0 {
    loop(x / 2)
  } else {
    loop(3 * x + 1)
  }
```

#### Function Expressions

Chirp has two ways to define functions inline,
as opposed to using [let] and [definition]s:

##### Fn Expressions
```
fn(x) = x + 1
fn f(x) = x == 0 || f(x - 1)
```
##### Lambda Expressions
```
λx.x
λk.λx.k
\x,y.y
\.10
```
Where `\` represents a lambda, but may be easier to type.
