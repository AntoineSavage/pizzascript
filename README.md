# Comments

The # symbol starts a single-comment, which ends at the end of the current line:
```
# This is a single-comment
```

There are no multi-line comments

# Data Types

There are nine data types:
- the empty type `()`
- number
- string
- symbol
- list
- dictionary
- struct
- function
- macro

The `typeof` operation can be used to obtain the type of a value:
```
(typeof ())         # -> ()
(typeof any_num)    # -> 'num
(typeof any_str)    # -> 'str
(typeof any_symb)   # -> 'symbol
(typeof any_list)   # -> 'list
(typeof any_dict)   # -> 'dict
(typeof any_struct) # -> 'struct, then call struc_*
(typeof any_func)   # -> 'func, then call func_*
(typeof any_macro)  # -> 'macro, then call macro_*
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

All numbers have type `'num`, which corresponds to the following operation `num`:
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
- `\u{D*}` : one or more hex digits representing a unicode codepoint (must be at most `\u{10FFFF}`)

All strings have type `'str`, which corresponds to the following variadic operation `str`:
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

All symbols have type `'symb`, which corresponds to the following operation `symb`:
```
(symb any_str)  # -> (parse_symb any_str)
(symb any_symb) # -> any_symb
(symb x)        # -> raise error
```

Symbols can be combined into *qualified* symbols (ex: `'Symb1.symb_2`)

Here are a few examples of qualified symbols:
```
'Module.function
''Package.Module.function
'''Package.Module.SubModule
''''my_dict.my_key1.my_key2
```

The following operations are used to merge and split qualified symbols:
```
(symb_split 'foo)     # -> ['foo]
(symb_merge 'foo [])  # -> 'foo

(symb_split 'foo.bar.baz)     # -> ['foo 'bar 'baz]
(symb_merge 'foo ['bar 'baz]) # -> 'foo.bar.baz

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

Any symbol can be converted back into an identifier by *unquoting* it, i.e. removing one or more quote (`'`) characters that are in front of it. Keep in mind that unquoting a symbol may lead to an undefined identifier error

In the macro section of this document, we will see how to quote and unquote programmatically

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

The preceding examples can also be expressed using the `list `operation:
```
(list)
(list 0 1 2 3 4 5)
(list "hello" "world" "!")
(list 'a 'b 'c 'd 'e)
(list (list) (list 1 2 3) (list "hello" 'world "!" ) )
```

All lists have type `'list`, which corresponds to the previously mentioned `list` operation

# Dictionaries

Dictionaries are immutable and heterogenous containers of key/value pairs

All types can be dictionary keys. Dictionaries store their key/value pairs as a sorted tree. The key ordering is as follows:
```
() < numbers < strings < symbols < lists < dictionaries < structs < functions < macros
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
    (<point (x 1) (y 2)> (func () []))
    ((func () ()) (macro 'evaluated ctx () [ctx ()]))
    ((macro 'evaluated ctx () [ctx ()]) ())
}
```

The preceding examples can also be expressed using the `dict `operation:
```
(dict)
(dict
    (() 0)
    (0 "")
    ("" 'dict)
    ('dict [])
    ([] {})
    ({} <point (x 1) (y 2)>)
    (<point (x 1) (y 2)> (func () []))
    ((func () []) ())
)
```

All dictionaries have type `'dict`, which corresponds to the previously mentioned `dict` operation

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

The preceding struct literal examples can also be expressed using the `struct `operation:
```
(struct void)
(struct one_of_each (mandat 123))
(struct one_of_each (mandat 123) (option 456))
(struct two_of_each (m1 1) (m2 2))
(struct two_of_each (m1 1) (m2 2) (o1 0) (o2 ""))
```

All structs have type `'struct`, which corresponds to the previously mentioned `struct` operation

Furthermore, the specific struct type can be obtained using the following operation:
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
(func_args list)  # -> 'args, i.e. variadic
```

Note that all functions arguments are evaluated left to right before being passed

# Macros

Macros are like advanced functions. They can be configured to quote/unquote their arguments, and they can be used for advanced context operations like identifier definition, error handling, etc

Here are examples of macro literals that behave exactly like functions:
```
(macro 'evaluated ctx ()    [ctx ()])         # nullary macro, returns ()
(macro 'evaluated ctx (x)   [ctx x])          # unary, returns its arg
(macro 'evaluated ctx (n m) [ctx (add n m)])  # binary, sums its args
# etc
(macro 'evaluated ctx args  [ctx args])       # variadic, returns its args
```

Thus, a macro has the following additional complexity:
- an agrument-passing strategy must be declared (ex: `'evaluated` in the above examples)
- an explicit context must be declared (ex: `ctx` in the above examples)
- the return value must be a size-2 list containing:
  - a (possibly modified) explicit context (ex: `ctx` in the above examples)
  - the effective return value (similar to a function's return value)

## Argument-passing strategy

The first argument in a macro literal is the argument-passing strategy, which must be one of the following symbols:
- `'evaluated`: the macro arguments must be evaluated left to right before being passed
- `'quoted`: the macro arguments must be quoted before before being passed
- `'deep_quoted`: the macro arguments must be deep-quoted before before being passed
- `'unquoted`: the macro arguments must be unquoted (and evaluated) left to right before being passed
- `'deep_unquoted`: the macro arguments must be deep-unquoted (and evaluated) left to right before being passed

Here are some examples of macros using such symbols:
```
# evaluates the arguments left to right
(macro 'evaluated ctx (x) [ctx x])

# quotes the arguments
(macro 'quoted ctx (x) [ctx x])

# deep-quotes the arguments
(macro 'deep_quoted ctx (x) [ctx x])

# unquotes (and evaluates) the arguments left to right
(macro 'unquoted ctx (x) [ctx x])

# deep-unquotes (and evaluates) the arguments left to right
(macro 'deep_unquoted ctx (x) [ctx x])
```

A macro's argument-passing strategy can be introspected like this:
```
(def macro_evaluated      (macro 'evaluated     ctx (x) [ctx x]))
(def macro_quoted         (macro 'quoted        ctx (x) [ctx x]))
(def macro_deep_quoted    (macro 'deep_qoted    ctx (x) [ctx x]))
(def macro_unquoted       (macro 'unquoted      ctx (x) [ctx x]))
(def macro_deep_unquoted  (macro 'deep_unquoted ctx (x) [ctx x]))

(macro_arg_pass_strat macro_evaluated)      # -> 'evaluated
(macro_arg_pass_strat macro_quoted)         # -> 'quoted
(macro_arg_pass_strat macro_deep_quoted)    # -> 'deep_qoted
(macro_arg_pass_strat macro_unquoted)       # -> 'unquoted
(macro_arg_pass_strat macro_deep_unquoted)  # -> 'deep_unquoted
```

### Evaluating arguments with `'evaluated`

Arguments are evaluated in two different ways:
- literals (ex: `()`, `0`, `""`, `'a`, `[]`, `{}`, `<void>`, `(func (x) x)`, `(macro 'evaluated ctx (x) [ctx x])`):
  - return the value represented by the literal
  - lists, dictionaries, and structs may need to evaluate their contents recursively
- identifiers (ex: `true`, `fib`):
  - return the value referenced by the identifier. Ex:
    - given: `(def my_value "hello")`
    - evaluating `my_value` returns `"hello"`
  - an undefined identifier error may be raised otherwise

Here is a more involved example:
```
# given:
(def my_macro (macro 'evaluated ctx (x) [ctx x])) # eq. to (func (x) x)
(def n 123)
(def my_value [ 0 "" 'a [] {} <void> (fac 5) n])

# when we evaluate my_value or the associated list literal:
(my_macro my_value)
(my_macro [ 0 "" 'a [] {} <void> (fac 5) n])

# both return the same thing:
# [ 0 "" 'a [] {} <void> 120 123]
# notice how (fac 5) and n were evaluated to 120 and 123, respectively
```
### Quoting macro arguments with `'quoted`

Macro arguments are quoted in two different ways:
- literals (ex: `()`, `0`, `""`, `'a`, `[]`, `{}`, `<void>`, `(func (x) x)`, `(macro 'evaluated ctx (x) [ctx x])`):
  - return the quoted version of the literal (see below)

- identifiers (ex: `true`):
  - return a matching symbol. Ex:
    - `true` becomes `'true`
    - `list` becomes `'list`

Here is a more involved example:
```
# given:
(def my_macro (macro 'quoted ctx (x) [ctx x]))
(def n 123)

# (fac 5) and n are evaluated here to 120 and 123 resp.
(def my_value [ 0 "" 'a [] {} <void> (fac 5) n])

# when we quote my_value:
(my_macro my_value)

# then returns the identifier, quoted (i.e. a symbol):
# 'my_value

# when, instead, we quote the associated list literal:
(my_macro [ 0 "" 'a [] {} <void> (fac 5) n])

# then returns the literal, quoted:
# ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated here, but quoted
```

### Deep-quoting macro arguments with `'deep_quoted`

Deep-quoting macro arguments is similar to simply quoting them, except for one main difference

Where quoting only works at the source-level (i.e. only works on literals and identifiers), encoding will first evaluate an identifier (i.e. replace it its associated value, this is the 'deep' in deep-quoting), and then quote said value as if it were a literal. Furthermore, deep-quoting a literal has the same effect as simply quoting it

In other words, macro arguments are deep-quoted in two different ways:
- literals (ex: `()`, `0`, `""`, `'a`, `[]`, `{}`, `<void>`, `(func (x) x)`, `(macro 'evaluated ctx (x) [ctx x])`):
  - return the quoted version of the literal (see below)

- identifiers (ex: `true`, `zero`, `fac`):
  - evaluate the identifier to obtain its associated value
  - return a the quoted version of this value (see below). Ex:
    - given: `(def my_func (func (x) x))`
    - deep-quoting `my_func`
      - evaluate identifier `my_func`, which gives us the function `(func (x) x)`
      - returns the quoted function `['func ['x] 'x]`

  - note that deep-quoting only ever replaces **one** identifier with its value. In order to perform recursive deep-quoting, you need to write a custom macro for it

Here is a more involved example:
```
# given:
(def my_macro (macro 'deep_quoted ctx (x) [ctx x]))
(def n 123)

# (fac 5) and n are evaluated here to 120 and 123 resp.
(def my_value [ 0 "" 'a [] {} <void> (fac 5) n])

# when we deep-quote my_value:
(my_macro my_value)

# then returns the referenced value, quoted:
# ['list 0 "" ''a ['list] ['dict] ['struct 'void] 120 123]
# notice how (fac 5) and n were already evaluated, therefore not quoted

# when, instead we deep-quote the associated list literal:
(my_macro [ 0 "" 'a [] {} <void> (fac 5) n])

# then returns the literal, quoted:
# ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated, but quoted
```

### Unquoting function arguments with `'unquoted`

Unquoting macro arguments is the reverse process of quoting them. Unquoting only works on number, string, symbol and list literals only. The resulting value is immediately evaluated. Errors may be raised by this process for various reasons (undefined identifier, malformed function call, etc). See below for details about unquoting listerals

Here is a more involved example:
```
# given:
(def my_macro (macro 'unquoted ctx (x) [ctx x]))
(def n 123)
(def my_code ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n])

# when we unquote 'my_code (symbol) or its associated list literal:
(my_macro 'my_code)
(my_macro ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n])

# then returns the literal, unquoted:
# [0 "" 'a [] {} <void> 120 123]
# notice how (fac 5) and n were evaluated to 120 and 123, respectively
```

### Deep-unquoting function arguments with `'unquoted`

Deep-unquoting macro arguments is similar to simply unquoting them, except for one main difference

Where unquoting only works at the source-level (i.e. only works on literals, and **not** identifiers), deep-unquoting will first evaluate an identifier (i.e. replace it its associated value, this is the 'deep' in deep-unquoting), and then unquote said value as if it were a literal. Furthermore, deep-unquoting a literal has the same effect as simply unquoting it. Deep-unquoting has the same type restrictions and consequences as normal unquoting

In other words, macro arguments are deep-unquoted in two different ways:
- literals (ex: `0`, `""`, `'a`, `[]`):
  - return the unquoted version of the literal (see below)

- identifiers (ex: `true`, `zero`, `fac`):
  - evaluate the identifier to obtain its associated value
  - return a the unquoted version of this value (see below). Ex:
    - given: `(def my_func ['func ['x] ['x]])`
    - deep-unquoting `my_func`
      - evaluate identifier `my_func`, which gives us the list `['func ['x] ['x]]`
      - returns the unquoted and evaluated function `(func (x) x)`

  - note that deep-unquoting only ever replaces **one** identifier with its value. In order to perform recursive deep-unquoting, you need to write a custom macro for it

Here is a more involved example:
```
# given:
(def my_macro (macro 'deep_unquoted ctx (x) [ctx x]))
(def n 123)
(def my_code ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n])

# when we deep-unquote my_code (identifier) or its associated list literal:
(my_macro my_code)
(my_macro ['list 0 "" ''a ['list] ['dict] ['struct 'void] ['fac 5] 'n])

# then returns the literal, unquoted:
# [0 "" 'a [] {} <void> 120 123]
# notice how (fac 5) and n were evaluated to 120 and 123, respectively
```

### Quoting literals

This operation is the reverse of unquoting literals (see below). Note that identifiers are **not** literals in any way. See deep-quoting for how they are quoted

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
  - first convert it to an evaluation literal using the corresponding operation. Ex:
    - `[1 2 3]` becomes `(list 1 2 3)`
    - `{(1 2) (3 4)}` becomes `(dict (1 2) (3 4))`
    - `<point (x 0) (y 0)>` becomes `(struct point (x 0) (y 0))`
  - then return the quoted version of this evaluation literal (see next point)

- evaluation literals (ex: `(list 1 2 3)`, `(dict (1 2) (3 4))`, `(struct point (x 0) (y 0))`, `(func (x) x)` and `(macro 'evaluated ctx (x) [ctx x])`):
  - return a list containing each element quoted recursively. Parentheses are treated as list brackets. Ex:
    - `(list 1 2 3)` becomes `['list 1 2 3]`
    - `(dict (1 2) (3 4))` becomes `['dict [1 2] [3 4]]`
    - `(struct point (x 0) (y 0))` becomes `['struct 'point ['x 0] ['y 0]]`
    - `(func (x) x)` becomes `['func ['n] 'n]`
    - `(macro 'evaluated ctx (x) [ctx x])` becomes `['macro ''evaluated 'ctx ['n] ['ctx 'n]]`
      - note the double-quote on the right-hand side, in `''evaluated`

### Unquoting literals

This operation is the inverse of quoting literals (see above). Note that identifiers are **not** literals in any way. See deep-unquoting for how they are unquoted

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

- evaluation literals (ex: `['list 1 2 3]`, `['dict [1 2] [3 4]]`, `['struct 'point ['x 0] ['y 0]]`, `['func ['x] 'x]` and `['macro ''evaluated 'ctx ['n] ['ctx 'n]]`):
  - first, unquote each element recursively, evaluating each from left to right
  - evaluate the resulting operation
  - raise an error if this operation is malformed

## Macros and contexts

A context is a dictionary made up of symbol keys each associated to a given value. Such values can be of any type. This context dictionary is initialized at the beginning of a module evaluation, and can be used and modified by macros

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

By default, a function or macro only has access to one implicit context, called the *function implicit context*

This *function implicit context* is the context of all identifiers (arguments, lambda closure, etc) available to the function or macro when it gets invoked. This context is determined when the function or macro is created. As a reminder, here are the two ways to create a function or macro:
- by using the `func` identifier in source code:
  - `(func (n) (add n 1)` or `(macro 'evaluated ctx (n) [ctx (add n 1)])`
  - the implicit context will contain all defined symbols up to that point in the module
  - in in particular, symbol `'add` must be defined up to that point in the module for the function or macro to succeed

- by unquoting a list starting with the `'func` or `'macro`symbol:
  - `['func ['n] ['add 'n 1]]` or `['macro ''evaluated 'ctx ['n] ['ctx ['add 'n 1]]]`
  - the implicit context will contain all defined in the context of the unquoting

A function's or macro's implicit context can be introspected like this:
```
(def add (func (x y) (...)))
(def compile (macro 'deep_unquoted ctx (x) [ctx x]))

# getting implicit context
(def my_add (func (n) (add n 1)))
(def my_add2 (compile ['func ['n] ['add 'n 1]]))

(func_get_ctx my_add)
(func_get_ctx my_add2)
# both return the following:
# {
#   (n ()) # argument, with default value
#   (add (func (x y) (...))) # lambda closure
# }

# setting implicit context
(def new_ctx { (n ()) (add my_add) })
(def my_add3 (func_set_ctx new_ctx my_add2))
```

The `def` operation seen up until now will also add the defined name to a function's own implicit context, thus allowing recursion to work as expected

For mutually-recursive functions, one can devise an operation `mutual` that takes a number of functions, and will add all of them in each of the implicit contexts.

### The macro explicit context

A macro receives an additional context argument, called the *macro explicit context*

This *macro explicit context* is the context of all definitions available to the *caller* of the macro. Since it is an explicit context, all of its keys must be qualified with the context name

Furthermore, a macro must return a size-2 list containing the explicit context (updated if needed), followed by the normal macro return value

Note that when using an explicit context, the identifier `'ctx` will be added to the macro's implicit context, unless such an identifier is already defined in the macro's implicit context

Here is an example of receiving an additional context:
```
# explicit caller context in identifier ctx
# evaluation semantics symbol must be provided
def my_macro (macro 'evaluated ctx (x y z)
  (print ctx.zero)
  (print ctx.hello)

  [ ctx         # unchanged explicit context
    (add x y z) # return value
  ]
))

# set-up the caller context
(def zero 0)
(def hello "hello")

# invoke macro
(macro 1 2 3) # -> 6
# prints: 0
# prints: "hello"

# the caller context was not modified
(print zero)  # prints: 0
(print hello) # prints: "hello"
```

Here is an example of modifying the explicit context:
```
# explicit caller context in identifier ctx
def my_macro (macro 'evaluated ctx (x y z)
  (print ctx.n)
  (print ctx.hello)

  # updated explicit context
  # contexts are just dictionaries
  (def out_ctx (set ctx.n (add ctx.n 1)))

  [ out_ctx     # updated explicit context
    (add x y z) # return value
  ]
))

# set-up the caller context
(def n 0)
(def hello "hello")

# invoke macro
(my_macro 1 2 3)
# prints: 0
# prints: "hello"

# caller context has changed
(print n) # prints: 1
```

## Other uses for the explicit context

Beyond simply manipulating the caller context, the *macro explicit context* can also be used to do the following:
- defining custom structs
- performing I/O
- requesting other module definition contexts
- aborting and resuming evaluation

They are implemented using *volatile variables*, which are variables that can be modified at any time by the interpreter. They should **not** be relied upon for anything other that what is specified in the following sections

### Defining custom structs

The *macro explicit context* can be used to define and create custom structs. This allows the implementation of features like:
- validating mandatory fields
- setting default values for optional fields

Operations like `'defstruct` and `'struct` (see section about custom structs) will read and update key `"__CUSTOM_STRUCTS_`, which contains the types, fields, etc, of structs that are defined in the current context

### Performing I/O

The *macro explicit context* can be used to open, read and write files. This allows the implementation of features like:
- read and write to the console
- read and write to disk
- send and receive data over the network

Assume a macro invokation `M` that receives an explicit context. Once `M` finishes, the interpreter will search the resulting explicit context for the key `'__REQUEST_IO__`. If found, the interpreter will do the following:
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
(def prompt1 (macro 'evaluated ctx (str)
  [ (put ctx '__REQUEST_IO__  ['std_out str])
    ()
  ]
))

# prompt 2/3: request the reading of the user string
(def prompt2 (macro 'evaluated ctx ()
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

The *macro explicit context* can be used to request other module definition contexts (i.e. the final context after a module has finished evaluating). This allows the implementation of features like:
- importing all or a subset of another module's definition
- aliasing, qualifying or hiding a subset of another module's definition

Assume a macro invokation `M` that receives an explicit context. Once `M` finishes, the interpreter will search the resulting explicit context for the key `'__REQUEST_MODULE_CTX__`. If found with a fully-qualified, single-quoted symbol `S`, the interpreter will do the following:
- remove this key from the explicit context
- search the module directory for the module `M` matching symbol `S`
- if module `M` is already evaluated, denote its final context as `C`
- if module `M` is not evaluated, evaluate it now, denoting its final context as `C`
- put a key `'__RESPONSE_MODULE_CTX__` with value `C` in the explicit context

Here is an example of requesting a module context:
```
(def req_data_list_ctx (macro 'evaluated ctx ()
  [ (put ctx '__REQUEST_MODULE_CTX__  'Data.List)
    ()
  ]
))

(req_data_list_ctx)
(print '__RESPONSE_MODULE_CTX__)
# prints: { ... }
```

### Aborting and resuming evaluation

The *macro explicit context* can be used to abort and resume evaluation. This allows the implementation of features like:
- error-handling with try/recover/finally
- forcefully return a value, thus bypassing API restrictions

Assume a macro invokation `M` that receives an explicit context. Once `M` finishes, the interpreter will search the resulting explicit context for the key `'__ABORT_EVAL_WITH__`. If found with any value `V`, the interpreter will do the following:
- look for a macro invokation `N` with the following properties:
  - nullary (i.e. zero-length argument list)
  - immediately follows `M` or the last aborted evaluation
- if such a macro invocation `N` is found, invoke it. Once `N` finishes, if this field has been removed from the explicit context, resume evaluation
- otherwise, abort evaluation, and move up the call stack until such a function invokation `N` is found
- if no such macro invokation `N` is ever found, the interpreter will halt with an message derived from `V`

Here is an example of aborting and resuming evaluation:
```
# this macro will abort evaluation
(def my_abort (macro 'evaluated ctx (abort_with)
  [ (put ctx '__ABORT_EVAL_WITH__  abort_with)
    ()
  ]
))

# abort then resume evaluation
(my_abort "Hello Crash World!")
(macro 'evaluated ctx ()
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

It is possible to prevent re-exporting identifiers by deleting their symbols from the current context at the end of the module. This can be done using `get_curr_ctx` and `set_curr_ctx`, which are implemented using the *macro explicit context*, like this:
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