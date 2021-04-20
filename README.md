# Comments

The # symbol starts a single-comment, which ends at the end of the current line:
```
# This is a single-comment
```

There are no multi-line comments

# Data Types

There are 8 data types:
- the empty type `()`
- number
- string
- symbol
- list
- dictionary
- struct
- function

The `typeof` function can be used to obtain the type of a value:
```
(typeof ())         # -> ()
(typeof any_num)    # -> 'num
(typeof any_str)    # -> 'str
(typeof any_symb)   # -> 'symbol
(typeof any_list)   # -> 'list
(typeof any_dict)   # -> 'dict
(typeof any_struct) # -> 'struct, then call struct_type
(typeof any_func)   # -> 'func, then call func_args, etc
```

# The empty type `()`

Has only one value: `()`

Used to represent the absence of any meaningful value

# Numbers

Integer literals (positive or negative):
```
0 1 2 3 4 5 10 123
-1 -2 -3 -4 -5 -10 -123
```

Floating-point number literals:
```
0.1 1.1 2.2 10.01 123.123
-1.1 -2.2 -3.3 -10.01 -123.123
1.23e123 -123E-123
```

All numbers have type `'num`, which corresponds to the following function `num`:
```
(num any_num)   # -> any_num
(num any_str)   # -> (parse_num any_str)
(num x)         # raise error
```
# Strings

Strings are represented by an array of unicode codepoints, encoded as utf-8

Strings cannot span multiple lines

Some simple string literals:
```
""                  # empty string
"abc123ACB_ !@#$%?" # ASCII printable characters
"àéèïîôöù"          # Unicode printable characters
```

Certain codepoints must be escaped:
- `\"` : double quote
- `\\` : backslash
- `\/` : forward slash
- `\b` : backspace
- `\f` : form feed
- `\n` : newline
- `\r` : carriage return
- `\t` : tab
- `\uDDDD` : four hex digits representing a unicode codepoint

All strings have type `'str`, which corresponds to the following variadic function `str`:
```
(str x)     # -> a nice, human-friendly representation
(str x y)   # -> apply tostr to x and y, and concat the result
(str x y z) # -> apply tostr to x, y and z, and concat the result
# etc
```

# Symbols

Symbols each represent a single value (themselves). They are formed by one or more quote (`'`) characters followed by either a letter or an underscore, followed by many letters, digits, or underscores

Here are some examples of symbol literals:
```
'a ''b ''c
'abc
'_foo ''BAR99 '''B4Z
```

All symbols have type `'symb`, which corresponds to the following function `symb`:
```
(symb any_str)  # -> (parse_symb any_str)
(symb any_symb) # -> any_symb
(symb x)        # -> raise error
```

Symbols can be combined into *qualified* symbols (ex: `'Symb1.symb_2`). The following functions are used to merge and split symbols:
```
(symb_split 'foo.bar.baz)      # -> ['foo 'bar 'baz]
(symb_merge 'foo ['bar 'baz])  # -> 'foo.bar.baz

# Number of quotes must match
(symb_merge ''foo [''bar ''baz])  # -> ''foo.bar.baz
(symb_merge 'foo [''bar '''baz])  # -> error
```

The number of quotes of a symbol can be obtained like this:
```
(symb_nbr_quotes 'foo) # -> 1
(symb_nbr_quotes ''bar) # -> 2
(symb_nbr_quotes '''foo.bar.baz) # -> 3
```

Booleans and other enum-like values can be represented using symbols like this:
```
(def false 'false)
(def true 'true)
```

Note that boolean logical gates (not, or, and) are implemented by the interpreter, since other constructs (if, cond, etc) use them

## Symbols and identifiers

Symbols have a close relationship with identifiers. An identifier is a symbol, but without any leading quote (`'`) characters

An identifier is just a name representing a previously defined value. In particular, an identifier is **not** a literal of any kind. For example, in the following:
```
(def my_value "hello world")
```
We can say that identifier `my_value` represents the value `"hello world"`, and its matching symbols look like `'my_value`

Here is a short list of identifiers, matching the previously mentioned symbols:
```
a b c
abc
_foo BAR99 B4Z
true false
```

Like symbols, identifiers can be *qualified*, to represent a directory-like hierarchy of values. Furthermore, we say that an identifier is *fully qualified* if represents the absolute 'path' to a vicen value in said directory-like hierarchy

Here are a few examples of qualified identifiers:
```
Module.function
Package.Module.function
Package.Module.SubModule
my_dict.my_key1.my_key2
```

Any identifier can be converted into a symbol by *quoting* it, i.e. adding one or more quote (`'`) characters in front of it

Any symbol can be converted back into an identifier by *unquoting* it, i.e. removing one or more quote (`'`) characters that are in front of it. Keep in mind that unquoting a symbol may lead to an undefined value error

In the function section of this document, we will see how to quote and unquote things automatically

# Lists

Lists are immutable and heterogenous value containers

Here are some examples of list literals:
```
[]
[0 1 2 3 4 5]
["hello" "world" "!"]
['a 'b 'c 'd 'e]
[ [] [1 2 3] ["hello" 'world "!" ] ]
```

The preceding examples can also be expressed using the `list `function:
```
(list)
(list 0 1 2 3 4 5)
(list "hello" "world" "!")
(list 'a 'b 'c 'd 'e)
(list (list) (list 1 2 3) (list "hello" 'world "!" ) )
```

All lists have type `'list`, which corresponds to the previously mentioned `list` function

# Dictionaries

Dictionaries are immutable and heterogenous containers of key/value pairs

All types can be dictionary keys. Dictionaries store their key/value pairs as a sorted tree. The key ordering is as follows:
```
() < numbers < strings < symbols < lists < dictionaries < structs < functions
```

Here are some examples of dictionary literals:
```
{}
{
    (() 0)
    (0 "")
    ("" 'dict)
    ('dict [])
    ([] {})
    ({} <point (x 1) (y 2)>)
    (<point (x 1) (y 2)> (func () [])))
    ((func () []) ())
}
```

The preceding examples can also be expressed using the `dict `function:
```
(dict)
(dict
    (() 0)
    (0 "")
    ("" 'dict)
    ('dict [])
    ([] {})
    ({} <point (x 1) (y 2)>)
    (<point (x 1) (y 2)> (func () [])))
    ((func () []) ())
)
```

All dictionaries have type `'dict`, which corresponds to the previously mentioned `dict` function

Dictionaries support a special syntax for obtaining and updating the value of symbol keys. Those symbol keys must be unqualified and single-quoted. The corresponding key/value pair must exist, otherwise an error is raised

For example:
```
# single key 'my_field (a symbol)
# single value "my_value" (a string)
(def my_dict {('my_field "my_value)}

# obtain value of symbol key
my_dict.my_field # -> "my_value"

# updating value of symbol key
# returns a new dictionary with the updated key/value pair
(set my_dict.my_field "new_value") # -> {('my_field "new_value)}
```

Dictionary also support the following standard, dynamic operations:
- obtain the list of keys
- get the value associated to a key (regardlesss of key type)
- put a key/value association, possibly adding it
- del a key/value association, possibly removing it

# Structs

Structs are similar to dictionaries in that they are immutable, heterogenous containers of key/value pairs. However, structs also differ from dictionaries in the following ways:
- struct definition must precede usage
- struct definition includes type, keys, and default values
- each struct type must be unique
- all keys in a struct are single-quoted, unqualified symbols
- struct keys cannot be added or removed

Each struct defines a set of mandatory keys, which must be provided at creation along with a value, as well as optional keys each with a default value in case that key/value pair is not not provided at creation

Here are some examples of struct definitions:
```
(defstruct void)
(defstruct one_of_each mandat (option 0))
(defstruct two_of_each m1 m2 (o1 0) (o2 ""))
```
Where `'mandat`, `'m1` and `'m2` are mandatory keys, and `'option`, `'o1` and `'o2` are optional keys

Here are some examples of struct literals:
```
<void>
<one_of_each (mandat 123)>  # 'option defaults to 0
<one_of_each (mandat 123) (option 456)>
<two_of_each (m1 1) (m2 2)> # 'o1 and 'o2 default to 0 and "" resp.
<two_of_each (m1 1) (m2 2) (o1 0) (o2 "")>
```

The preceding struct literal examples can also be expressed using the `struct `function:
```
(struct void)
(struct one_of_each (mandat 123))
(struct one_of_each (mandat 123) (option 456))
(struct two_of_each (m1 1) (m2 2))
(struct two_of_each (m1 1) (m2 2) (o1 0) (o2 ""))
```

All structs have type `'struct`, which corresponds to the previously mentioned `struct` function

Furthermore, the specific struct type can be obtained by calling the following function:
```
(struct_type <void>) # -> 'void
```

Structs, like dictionaries, support a special syntax for obtaining and updating the value of keys. The key/value pair must exist, otherwise an error is raised

For example:
```
# single key 'my_field
# single value "my_value"
(defstruct my_struct_type my_field))
(def my_struct_val <my_struct_type (my_field "my_value")>)

# obtain value for a given key
my_struct_val.my_field # -> "my_value"

# updating value for a given key
# returns a new struct with the updated key/value pair
(set my_struct_val.my_field "new_value")
# -> <my_struct_type (my_field "new_value)>
```

Structs also support the the following dictionary dynamic operations:
- obtain the list of keys
- get the value associated to a key
  - raise error if key does not exist
- put a key/value association
  - raise error if key does not exist

# Functions

Here are examples of function literals:
```
(func () ())            # nullary, returns ()
(func (x) x)            # unary, returns its arg
(func (n m) (add n m))  # binary, sums its args
# etc
(func args args)        # variadic, returns its args
```

Here are some examples of useful functions:
```
(def id (func (x) x))
(def apply (func (f x) (f x)))
(def flip (func (f x y) (f y x)))
(def cmp (func (f g) (func (x) (f (g x)))))
```

A function's argument list can be introspected like this:
```
(func_args id)    # -> ['id]
(func_args apply) # -> ['f 'x]
(func_args flip)  # -> ['f 'x 'y]
(func_args cmd)   # -> ['f 'g]
```

A function can be annotated to add the following extra functionality:
- Changing the evaluation semantics of the function
- Explicitely using and modifying the call-site context
  - mutability, I/O, concurrency, errors, etc

## Evaluation semantics

Functions can be annotated with one of the following symbols:
- `'by_value` (default): evaluates the function arguments (left-to-right)
- `'by_name`: quotes the function arguments
- `'by_code`: decompiles the function arguments
- `'compile`: compiles (and evaluates) the function arguments (left-to-right)

Here are some examples of functions using such symbols:
```
# evaluates the function arguments
# the following are equivalent
(func (x) x)
(func 'by_value (x) x)

# quotes the function arguments
(func 'by_name (x) x)

# decompiles the function arguments
(func 'by_code (x) x)

# compiles (and evaluates) the function arguments
(func 'compile (x) x)
```

A function's evaluation semantics can be introspected like this:
```
(def func_default   (func (x) x))
(def func_by_value  (func 'by_value (x) x))
(def func_by_name   (func 'by_name (x) x))
(def func_by_code   (func 'by_code (x) x))
(def func_compile   (func 'compile (x) x))

(func_eval_sem func_default)  # -> 'by_value
(func_eval_sem func_by_value) # -> 'by_value
(func_eval_sem func_by_name)  # -> 'by_name
(func_eval_sem func_by_code)  # -> 'by_code
(func_eval_sem func_compile)  # -> 'compile
```

### Evaluating function arguments with `'by_value`

A function evaluates its argument in two different ways:
- literals (ex: `()`, `0`, `""`, `'a`, `[]`, `{}`, `<void>`, `(func (x) x)`):
  - return the value represented by the literal
  - lists, dictionaries, structs, and functions may need to evaluate their contents recursively
- identifiers (ex: `true`, `fib`):
  - return the value referenced by the identifier. Ex:
    - given: `(def my_value "hello")`
    - evaluating `my_value` returns `"hello"`

Here is a more involved example:
```
# given:
(def my_func (func (x) x)) # eq. to (func 'by_value (x) x)
(def n 123)
(def my_value [ 0 "" 'a [] {} <void> (fac 5) n])

# when we evaluate my_value or the associated list literal:
(my_func my_value)
(my_func [ 0 "" 'a [] {} <void> (fac 5) n])

# both return the same thing:
# [ 0 "" 'a [] {} <void> 120 123]
# notice how (fac 5) and n were evaluated to 120 and 123, respectively
```
### Quoting function arguments with `'by_name`

A function quotes its arguments in two different ways:
- literals (ex: `()`, `0`, `""`, `'a`, `[]`, `{}`, `<void>`, `(func (x) x)`):
  - return the quoted version of the literal (see below)

- identifiers (ex: `true`):
  - return a matching symbol. Ex:
    - `true` becomes `'true`
    - `list` becomes `'list`

Here is a more involved example:
```
# given:
(def my_func (func 'by_name (x) x))
(def n 123)
(def my_value [ 0 "" 'a [] {} <void> (fac 5) n])

# when we quote my_value:
(my_func my_value)

# then returns the identifier, quoted (i.e. a symbol):
# 'my_value

# when, instead, we quote the associated list literal:
(my_func [ 0 "" 'a [] {} <void> (fac 5) n])

# then returns the literal, quoted:
# ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated, but quoted
```
### Decompiling function arguments with `'by_code`

Decompiling function arguments is somewhat similar to quoting them, except for one main difference

Where quoting only works at the source-level (i.e. only works on literals and identifiers), decompiling will first replace an identifier with its associated value, and then quote said value as if it were a literal. Furthermore, decompiling a literal has the same effect as quoting it

In other words, a function decompiles its arguments in two different ways:
- literals (ex: `()`, `0`, `""`, `'a`, `[]`, `{}`, `<void>`, `(func (x) x)`):
  - return the quoted version of the literal (see below)

- identifiers (ex: `true`, `zero`, `fac`):
  - replace the identifier with its associated value
  - return a the quoted version of this value (see below). Ex:
    - given: `(def my_func (func (x) x))`
    - decompiling `my_func`
      - replace identifier `my_func` with function `(func (x) x)`
      - returns the quoted function `['func ['x] 'x]`

  - note that decompilation only ever replaces **one** identifier with its value at a time. In order to perform recursive decompilation, you need to write a custom function for it

Here is a more involved example:
```
# given:
(def my_func (func 'by_code (x) x))
(def n 123)
(def my_value [ 0 "" 'a [] {} <void> (fac 5) n])

# when we decompile my_value or its associated list literal:
(my_func my_value)
(my_func [ 0 "" 'a [] {} <void> (fac 5) n])

# then returns the referenced value, quoted:
# ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated, but quoted
```

### Compiling function arguments with `'compile`

Compiling function arguments is the reverse operation of decompiling them. Compiling will first replace an identifier with its associated value, and then unquote said value as if it were a literal. Furthermore, decompiling a literal results in unquoting it. The resulting unquoted value is then evaluated. Errors may be raised by this process for various reasons (undefined identifier, malformed function call, etc)

In other words, a function compiles its arguments in two different ways:
- quoted literals (ex: `()`, `0`, `""`, `''a`, `'a`, `['func ['x] 'x]`):
  - return the unquoted version of the literal (see below)

- identifiers (ex: `true`, `zero`, `fac`):
  - replace the identifier with its associated value
  - return a the unquoted version of this value (see below). Ex:
    - given: `(def my_code ['func ['x] 'x]`
    - compiling `my_code`
      - replace identifier `my_code` with list `['func ['x] 'x]`
      - returns the unquoted and evaluated function `(func (x) x)`

  - note that compilation only ever replaces **one** identifier with its value at a time. In order to perform recursive compilation, you need to write a custom function for it

Here is a more involved example:
```
# given:
(def my_func (func 'compile (x) x))
(def n 123)
(def my_code ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n])

# when we compile my_code or its associated list literal:
(my_func my_code)
(my_func ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n])

# then returns the referenced value, unquoted:
# [0 "" 'a [] {} <void> 120 123]
# notice how (fac 5) and n were evaluated to 120 and 123, respectively
```

### Quoting literals

This operation is the reverse of unquoting literals

Each literal can be quoted in the following ways:
- literals for the empty type `()`, numbers and strings:
  - return their own value. Ex:
    - `()` becomes `()`
    - `123` becomes `123`
    - `"hello"` becomes `"hello"`

- symbol literals (ex: `'true`):
  - return a new symbol literal with one more quote (`'`) character prepended to it. Ex:
    - `'fib` (one quote) becomes `''fib` (two quotes)
    - `''list` (two quotes) becomes `'''list` (three quotes)

- lists, dictionaries and struct literals (ex: `[]`, `{}`, `<void>`):
  - first convert it to a function literal using the corresponding function call. Ex:
    - `[1 2 3]` becomes `(list 1 2 3)`
    - `{(1 2) (3 4)}` becomes `(dict (1 2) (3 4))`
    - `<point (x 0) (y 0)>` becomes `(struct point (x 0) (y 0))`
  - then return the quoted version of this function literal (see next point)

- function literals (ex: `(func (x) x)`):
  - return a list containing each element quoted recursively. Parentheses are treated as list brackets for this operation. Ex:
    - `(func (x) x)` becomes `['func ['n] 'n]`
    - `(list 1 2 3)` becomes `['list 1 2 3]`
    - `(dict (1 2) (3 4))` becomes `['dict [1 2] [3 4]]`
    - `(struct point (x 0) (y 0))` becomes `['struct 'point ['x 0] ['y 0]]`

### Unquoting literals

This operation is the inverse of quoting literals

Each literal can be unquoted in the following ways:
- the empty type `()`, numbers and strings:
  - return their own value. Ex:
    - `()` becomes `()`
    - `123` becomes `123`
    - `"hello"` becomes `"hello"`

- symbol literals with two or more quotes (ex: `''true`):
  - return a new symbol literal with one less quote (`'`) character prepended to it. Ex:
    - `''fib` (two quotes) becomes `'fib` (one quotes)
    - `'''list` (three quotes) becomes `''list` (two quotes)

- symbol literals with one quote (ex: `'true`):
  - remove the quote (`'`) character to obtain an identifier
  - return the value associated to this identifier
  - raise an error if this identifier is not defined

- list-function literals (ex: `['func ['x] 'x]`):
  - first, unquote each element recursively, evaluating each from left to right
  - evaluate the resulting function call
  - raise an error if this function call is malformed

## Functions and contexts

A context is a dictionary made up of symbol keys each associated to a given value. Such values can be of any type. This context dictionary is initialized at the beginning of a module evaluation, and can be used and modified by functions annotated to do so (explained in this section)

A context is said to be implicit when its keys can be referenced without being qualified by the context name, as simple identifiers. Conversely, a context is said to be explicit when its keys must be qualified with the context name, resulting in a qualified identifier. For example:
```
# add and get key 'zero with implicit context
# key 'zero is not qualified
(def zero 0)
(print zero)

# ctx is an explicit context
(def ctx {(zero 0)})

# get key 'zero from explicit context
# key 'ctx.zero is qualified with the context
(print ctx.zero)
```

### The function implicit context

By default, a function only has access to one implicit context, called the *function implicit context*

This *function implicit context* is the context of all identifiers (arguments, lambda closure, etc) available to the function when it gets invoked. This context is determined when the function is created. As a reminder, here are the two ways to create a function:
- by using the `func` identifier in source code:
  - `(func (n) (add n 1)`
  - the implicit context will contain all defined symbols up to that point in the module
  - in in particular, symbol `'add` must be defined up to that point in the module for the function to succeed

- by compiling a list starting with the `'func` symbol:
  - `['func ['n] ['add 'n 1]]`
  - the implicit context will be empty after being compiled like this
  - an implicit context must be set before the function gets invoked

A function's implicit context can be introspected like this:
```
(def add (func (x y) (...)))
(def compile (func 'compile (x) x))

# implicit context initialized automatically
(def my_add (func (n) (add n 1)))
(func_get_ctx my_add)
# -> {
#   (n ()) # argument, with default value
#   (add (func (x y) (...))) # lambda closure
# }

# Implicit context empty and must be initialized
(def my_add2 (compile ['func ['n] ['add 'n 1]]))
(func_get_ctx my_add2) # -> {}

(def new_ctx { (n ()) (add add) })
(def my_add_with_ctx (func_set_ctx new_ctx my_add2))
```

The `def` operation seen up until now will also add the defined name to a function's implicit context, thus allowing recursion to work as expected

For mutually-recursive functions, one can devise an operation `mutual` that takes a number of functions, and will add all of them in each of the implicit contexts.

### The function explicit context

A function can be annotated to receive an additional context argument, called the *function explicit context*

This *function explicit context* is the context of all definitions available to the *caller* of the function. Since it is an explicit context, all of its keys must be qualified with the context name

Note that if an explicit context is used, then a function evaluation semantics symbol MUST be provided between it and arguments, otherwise, the context identifier will be interpreted as the function's arguments, considered normal arguments, and the function arguments will be interpreted to be part of the function body

When an explicit context is received, the function return value must be a size-2 list containing the explicit context (updated if needed), followed by the normal function return value

Here is an example of receiving an additional context:
```
# explicit caller context in identifier ctx
# evaluation semantics symbol must be provided
def my_func (func ctx 'by_value (x y z)
  (print ctx.zero)
  (print ctx.hello)

  # return unchanged explicit context and value
  [ctx (add x y z)]
))

# set-up the caller context
(def zero 0)
(def hello "hello")

# invoke function
(my_func 1 2 3) # -> 6
# prints: 0
# prints: "hello"

# the caller context was not modified
(print zero)  # prints: 0
(print hello) # prints: "hello"
```

Here is an example of receiving *and modifying* an additional context:
```
# explicit caller context in identifier ctx
# evaluation semantics symbol must be provided
def my_func (func ctx 'by_value (x y z)
  (print ctx.n)
  (print ctx.hello)

  # updated explicit context
  # contexts are just dictionaries
  (def out_ctx (set ctx.n (add ctx.n 1)))

  # return updated explicit context and value
  [out_ctx (add x y z)]
))

# set-up the caller context
(def n 0)
(def hello "hello")

# invoke function
(my_func 1 2 3)
# prints: 0
# prints: "hello"

# caller context has changed
(print n) # prints: 1
```

## Other uses for the explicit context

Beyond simply manipulating the caller context, the *function explicit context* can also be used to do the following:
- performing I/O
- requesting other module definition contexts
- aborting and resuming evaluation

They are implemented using *volatile variables*, which are variables that can be modified at any time by the interpreter. They should **not** be relied upon for anything other that what is specified in the following sections

### Performing I/O

The *function explicit context* can be used to open, read and write files. This allows the implementation of features like:
- read and write to the console
- read and write to disk
- send and receive data over the network

Assume a function invokation `F` that receives an explicit context. Once `F` finishes, the interpreter will search the resulting explicit context for the key `'__REQUEST_IO__`. If found, the interpreter will do the following:
- analyze the associated value for the type of I/O to perform. Ex:
  - for writing `"Hello World"` to the console, the value could be: `['std_out, "Hello World!"]`
  - for reading from the console, the value could be: `'std_in`
  - etc

- remove this key from the explicit context
- perform the requested I/O operation, noting the result as `RESULT`
- put a key `'__RESPONSE_IO__` with value `RESULT`

Here is an example of reading and writing to the console:
```
# prompt 1/3: request the writing of the prompt string
(def prompt1 (func ctx 'by_value (str)
  [ (put ctx '__REQUEST_IO__  ['std_out str])
    ()
  ]
))

# prompt 2/3: request the reading of the user string
(def prompt2 (func ctx 'by_value ()
  # (get ctx '__RESPONSE_IO__) # ignore 'std_out response
  [ (put ctx '__REQUEST_IO__ 'std_in)
    ()
  ]
))

# prompt 3/3: process response
(prompt1 "Enter your name: ")
# prints: Enter your name: 

(prompt2)
# user enters their name: John

(print __RESPONSE_IO__)
# prints: John
```

### Requesting other module definition contexts

The *function explicit context* can be used to request other module definition contexts (i.e. the final context after a module has finished evaluating). This allows the implementation of features like:
- importing all or a subset of another module's definition
- aliasing, qualifying or hiding a subset of another module's definition

Assume a function invokation `F` that receives an explicit context. Once `F` finishes, the interpreter will search the resulting explicit context for the key `'__REQUEST_MODULE_CTX__`. If found with a fully-qualified, single-quoted symbol `S`, the interpreter will do the following:
- remove this key from the explicit context
- search the module directory for the module `M` matching symbol `S`
- if module `M` is already evaluated, denote its final context as `C`
- if module `M` is not evaluated, evaluate it now, denoting its final context as `C`
- put a key `'__RESPONSE_MODULE_CTX__` with value `C` in the explicit context

Here is an example of requesting a module context:
```
(def req_data_list_ctx (func ctx 'by_value ()
  [ (put ctx '__REQUEST_MODULE_CTX__  'Data.List)
    ()
  ]
))

(req_data_list_ctx)
(print '__RESPONSE_MODULE_CTX__)
# prints: { ... }
```

### Aborting and resuming evaluation

The *function explicit context* can be used to abort and resume evaluation. This allows the implementation of features like:
- error-handling with try/recover/finally
- forcefully return a value, thus bypassing API restrictions

Assume a function invokation `F` that receives an explicit context. Once `F` finishes, the interpreter will search the resulting explicit context for the key `'__ABORT_EVAL_WITH__`. If found with any value `V`, the interpreter will do the following:
- look for a function invokation `G` with the following properties:
  - nullary (i.e. zero-length argument list)
  - receivees an explicit context
  - immediately follows `F` or the last aborted evaluation
- if such a function invocation `G` is found, invoke it. Once `G` finishes, if this field has been removed from the explicit context, resume evaluation
- otherwise, abort evaluation, and move up the call stack until such a function invokation `G` is found
- if no such function invokation `G` is ever found, the interpreter will halt with an message derived from `V`

Here is an example of aborting and resuming evaluation:
```
# this function will abort evaluation
(def my_abort (func ctx 'by_value (abort_with)
  [ (put ctx '__ABORT_EVAL_WITH__  abort_with)
    ()
  ]
))

# abort then resume evaluation
(my_abort "Hello Crash World!")
(func ctx 'by_value ()
  [ (del ctx '__ABORT_EVAL_WITH__)
    ()
  ]
)

# abort evaluation and halting the interpreter
(my_abort "Hello Crash World!")
# prints: "Hello Crash World!"
```

# Modules

Source code can be split in multiple files. Each file corresponds to a module, and the path of a file corresponds to the module identifier. In particular, the relative path from the source root of any given file corresponds to the fully-qualified identifier of the corresponding module. For example, the following file structure:
```
src/
  main.pz       # module 'main
  Ast.Ast       # module 'data
  data/
    set.pz      # module 'data.set
    list.pz     # module 'data.list
    list/
      lazy.pz   # module 'data.list.lazy
```

## Imports

In order for a module to use identifiers defined by another module, it first needs to `import` those identifiers. The operation for importing identifiers from a module requires the fully-qualified identifier of the module to import, and simply creates a dictionary structure in the current context to hold the new identifiers

Here are some example of imports and symbol manipulations:
```
# import a module
(import data.list)
(data.list.concat [1 2 3] [4 5 6]) # -> [1 2 3 4 5 6]
(data.list.zip [1 2 3] [4 5 6]) # -> [[1 4] [2 5] [3 6]]

# define alias
(def list data.list)
(list.concat [1 2 3] [4 5 6]) # -> [1 2 3 4 5 6]
(list.zip [1 2 3] [4 5 6]) # -> [[1 4] [2 5] [3 6]]

# expose specific identifiers
(def zip list.zip)
(list.concat [1 2 3] [4 5 6]) # -> [1 2 3 4 5 6]
(zip [1 2 3] [4 5 6]) # -> [[1 4] [2 5] [3 6]]
```

## Exports and private identifiers

By default, every definition in a module `M` will be exported (i.e. made available to all other modules importing `M`). This includes identifiers defined in the module itself, as well as those imported from other modules (transitive imports, or re-exports)

In other words, if module `A` defines identifier `a`, and if module `B` defines identifier `b` and imports module `A`, and if module `C` imports module `B` only and defines nothing itself, then module `C` will have access to identifiers `a` and `b`

It is possible to prevent re-exporting identifiers by deleting their symbols from the current context at the end of the module. This can be done using `get_curr_ctx` and `set_curr_ctx`, which are implemented using the *function explicit context*, like this:
```
# main.pz

# a private definition, should be made available
(def _private "I'm private!")

# a public definition, should be made available
(def public_func (func () _private))

# ...

# get the current context, i.e. a dictionary
(def ctx (get_curr_ctx))

# delete '_private to make it unavailable
# this does not impact any previous use of '_private
(def new_ctx (del ctx '_private)_

# this will be forgotten by next statement
(def forgotten "This will be forgotten!")

# overwrite the current context
(set_curr_ctx new_ctx)
```

## The `base` module

There exists a module named `base`, of which all identifiers are automatically accessible to all modules. This module contains identifiers that are widely used and have no other dependencies than themselves. Think of it like if every module started with the following:
```

# this makes (base.add 1 2) possible
# i.e. base is a dictionary
(import base)

# obtain current context
# contexts are dictionaries
(def ctx (get_curr_ctx))

# add all base keys to current context
# this makes (add 1 2) possible
# also (base.add 1 2) is still possible if needed
(def new_ctx (merge ctx base)

# overwrite current context
(set_curr_ctx new_ctx)
```

## Module evaluation order

The program evaluation algorithm is as follows:
1. let `modules` be `{}`, such that:
  - each key is a module's fully-qualified, single-quoted symbol. Ex:
    - `'main`
    - `'data.list.lazy`

  - each value is a module's context dictionary. Ex:
    - `{}` 

2. let `(evaluate modules module)` be defined (see below)
3. call `(evaluate modules 'main)`
4. halt

The `(evaluate modules module)` function algorithm is as follows:
1. if `modules` already contains `module`: return
2. compile `module` into a list of unevaluated expressions `es`
3. let `ctx` be: `{}`
4. let `modules` now be: `(put modules module {})`
5. evaluate `module` using: `ctx`
    1. evaluate each expression `e` in `es`
        1. keeping track of modifications to `ctx`
        2. no need to update `modules` on each modification

    2. if `e` access the context of another module `m2`:
        1. let `modules` now be: `(put modules module ctx)`
        2. call `(evaluate modules m2)`
        3. finish evaluating `e`

6. when `module` is fully evaluated with a final context `ctx`:
    1. let `modules` now be: `(put modules module ctx)`

This algorithm ensures that:
  - required (and only required) modules are ever compiles and evaluated
  - circular dependencies are allowed and will not cause an infinite recursion
    - they MAY cause undefined identifier errors, ex:
      - module `A` imports module `B`, in which module `B` imports module `A`
      - mdoule `B` will not have access to identifiers of `A` defined after its module `A`'s `(import B)` statement
      - this is because evaluation of `A` is paused until module `B` finishes evaluating

    - this can be fixed by doing one of the following:
      - creating a `friend zone` in A and B, where code required for both to evaluate correctly comes before the import statement of the other
      - moving dependencies to a new module