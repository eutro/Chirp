# Coding Research Project - Design Outline

## What we want to cover:

- Type systems
  - Type inference


- Memory Management (Automatic mm not manual)
  - Garbage Collection
    - Algorithms, implementations


- Programming Paradigms
   - Imperative or functional


- Runtime
  - Compiled 
  - Garbage Collector

- Syntax
- Static or dynamic typing
- Performance or readability
- What is it going to be used for?
- Build a standard library

## Implementation

- C++
- LLVM

## Steps
- First start with basics like integer,float, mul/div, add/sub ?
- For compiler:
 - Lexer (Should take a couple of hundred lines)
 - Parser (Should take a couple of hundred lines)
   - Decide on grammar rules etc
 - Abstract syntax tree
 - Machine code

## Design
- Paradigms
  - Functional

## Syntax Draft

See [tokens](spec/tokens.txt) and [grammar](spec/grammar.txt).

### Examples:

- Declare a function `foo` which adds 1 to a number.

1. No type hints:

```
defn foo(x) = x + 1
```

2. With type hints:

```
defn foo(x: int): int = x + 1
```

---

- Declare a variable `foo` with a value of 1:

```
defn foo = 1
```

---

- Define a factorial function:

1. Recursively:

```
defn factorial(x) = if x <= 1 { 1 } else { x * factorial(x - 1) }
```

2: Iteratively:

```
defn factorial(x) =
  let fact = 1, i = 2
    in loop ( if i > x { fact } else { loop(fact * i, i + 1) } )
```
