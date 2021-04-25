# Comments

The # symbol starts a single-comment, which ends at the end of the current line:
```
# This is a single-comment
```

There are no multi-line comments

# Forms

Parentheses (`(` and `)`) denote forms (i.e. a function invokation). For example:
```
(f)       # invokes 'f with zero arguments
(f x)     # invokes 'f with one argument 'x
(f x y)   # invokes 'f with two argument 'x and 'y
(f x y z) # invokes 'f with three arguments 'x, 'y and 'z
# etc
```

# Data Types

There are seven data types:
- the unit type `()`, which has a single value: `()`
- number
- string
- symbol
- list
- dictionary
- function

The `typeof` function can be used to obtain the type of a value:
```
(typeof ())         # -> ()
(typeof any_num)    # -> 'num
(typeof any_str)    # -> 'str
(typeof any_symb)   # -> 'symb
(typeof any_list)   # -> 'list
(typeof any_dict)   # -> 'dict
(typeof any_func)   # -> 'func
```

Additionally, the following functions work on all types:
```
(eq x y)  # -> 'true | 'false. Whether x and y are equal
# values of different types are automatically different

(lt x y)  # -> 'true | 'false. Whether x is less than y
# values of different types are ordered as such:
# () < any_num < any_str < any_symb < any_list < any_dict < any_func
```

# The unit type `()`

Has only one value: `()`, which is also its own type

Used to represent the absence of any meaningful value. Has no meaningful functions associated with it

# Numbers

Numbers can be ints (arbitrary size) or floats (64-bit)

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

All numbers have type `'num`, which corresponds to the following functions `num`:
```
(num any_num)   # -> any_num
(num any_str)   # -> converts the string to a number
(num x)         # raise error
```

The following functions work on numbers:
```
(add x y)
(sub x y)
(mult x y)
(div x y)
(mod x y)
(exp x y)
(log x y)
```

# Strings

Strings are represented by an array of unicode codepoints

Strings cannot span multiple lines

Some simple string literals:
```
""                  # empty string
"abc123ACB_ !@#$%?" # some ASCII printable characters
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

All strings have type `'str`, which corresponds to the following variadic function `str`:
```
(str x)     # -> a nice, human-friendly representation (not meant to be parsed)
(str x y)   # -> concat the results of (str x) and (str y)
(str x y z) # -> concat the results of (str x), (str y) and (str z)
# etc
```

The following functions work on strings:
```
(str_size "")    # -> 0
(str_size "abc") # -> 3
```

# Symbols

Symbols each represent a single value (themselves). They are formed by one or more quote (`'`) characters followed by either a letter or an underscore, followed by many letters, digits, or underscores

Here are some symbol literals:
```
'a ''b '''c
'abc
'_foo ''BAR99 '''B4Z
```

All symbols have type `'symb`, which corresponds to the following function `symb`:
```
(symb any_str)  # -> converts the string to a symbol
(symb any_symb) # -> any_symb
(symb x)        # -> raise error
```

Symbols can be combined into *qualified* symbols (ex: `'Symb1.symb_2`)

Here are a few qualified symbols:
```
'Module.function
''Package.Module.function
'''Package.Module.SubModule
''''my_dict.my_key1.my_key2
```

The following functions are used to manipulate symbols:
```
(symb_split 'foo)         # -> ['foo]
(symb_split 'foo.bar.baz) # -> ['foo 'bar 'baz]

(symb_merge 'foo)               # -> 'foo
(symb_merge 'foo 'bar 'baz)     # -> 'foo.bar.baz
(symb_merge 'foo ['bar] ['baz]) # -> 'foo.bar.baz

# Number of quotes must match
(symb_merge ''foo ''bar ''baz)  # -> ''foo.bar.baz
(symb_merge 'foo ''bar '''baz)  # -> error
```

The number of quotes of a symbol can be obtained like this:
```
(symb_nbr_quotes 'foo)  # -> 1
(symb_nbr_quotes ''bar) # -> 2
(symb_nbr_quotes '''foo.bar.baz) # -> 3
```

Booleans and other enum-like values are implemented using symbols like this:
```
(def false 'false)
(def true 'true)
```

## Symbols and identifiers

Symbols have a close relationship with identifiers. An identifier is a symbol, but without any leading quote. In other words, a symbol is a quoted identifier

An identifier is just a name representing a previously defined value. In particular, an identifier is **not** a literal of any kind, or is **not** itself a value of any kind. In order to obtain the value associated to the identifier, it must be evaluated. For example, in the following:
```
(def my_value "hello world")
```
We can say that identifier `my_value` represents the value `"hello world"`, and its matching symbols look like `'my_value`. When evaluating identifier `my_value`, it will produce value `"hello world"`

Here are some identifiers, matching the previously mentioned symbols (i.e. without quotes):
```
a b c
abc
_foo BAR99 B4Z
true false
```

Like symbols, identifiers can be *qualified*, to represent a hierarchy of values

Here are some qualified identifiers:
```
Module.function
Package.Module.function
Package.Module.SubModule
my_dict.my_key1.my_key2
```

Any identifier can be converted into a symbol by *quoting* it, i.e. adding one or more leading quote

Conversely, any symbol can be converted into an identifier by *unquoting* it, i.e. removing one or more leading quote and evaluating the result. Keep in mind that unquoting a symbol may lead to an undefined identifier error

Note that a symbol by itself is not required to correspond to a previously defined identifier. Only the unquoting process requires that relationship between symbols and identifiers. For example, symbol `'foo` is a perfectly valid symbol and can be freely passed around, even places where identifier `foo` is not defined

We will see later on in this document how to programmatically quote and unquote identifiers and symbols, and how to determine if a symbol represents a previously defined identifier or not

# Lists

Lists are immutable and heterogenous value containers

Here are some list literals:
```
[]
[0 1 2 3 4 5]
["hello" "world" "!"]
['a 'b 'c 'd 'e]
[ [] [1 2 3] ["hello" 'world "!" ] ]
```

The preceding examples can also be expressed using the `list` function:
```
(list)
(list 0 1 2 3 4 5)
(list "hello" "world" "!")
(list 'a 'b 'c 'd 'e)
(list (list) (list 1 2 3) (list "hello" 'world "!" ) )
```

The square bracket notation (i.e. using `[` and `]`) is useful for cleaning-up the source code, while the `list` function is useful to represent a list as a form, i.e. while quoting and unquoting

All lists have type `'list`, which corresponds to the previously mentioned `list` function

The following functions are used to manipulate lists:
```
(list_is_empty [])      # -> 'true
(list_is_empty [1 2 3]) # -> 'false

(list_get_size [])      # -> 0
(list_get_size [1 2 3]) # -> 3

(list_head [])      # -> error
(list_head [1 2 3]) # -> 1

(list_tail [])      # -> error
(list_tail [1 2 3]) # -> [2 3]
```

# Dictionaries

Dictionaries are immutable and heterogenous containers of key/value pairs. All types can be keys in dictionaries

Here are some dictionary literals:
```
{}
{ (1 2) (3 4) }
{
    (() 0)
    (0 "")
    ("" 'dict)
    ('dict [])
    ([] {})
    ({} (func () ()))
    ((func () ()) ())
}
```

The preceding examples can also be expressed using the `dict` function:
```
(dict)
(dict
    (() 0)
    (0 "")
    ("" 'dict)
    ('dict [])
    ([] {})
    ({} (func () ()))
    ((func () ()) ())
)
```

The curly brace notation (i.e. using `{` and `}`) is useful for cleaning-up the source code, while the `dict` function is useful to represent a dictionary as a form, i.e. while quoting and unquoting

All dictionaries have type `'dict`, which corresponds to the previously mentioned `dict` function

## Dictionaries and qualified identifiers

Dictionaries also have a special relationship with qualified identifiers. If a dictionary contains a key that is an unqualified, single-quoted symbol, then it can be accessed by evaluating the corresponding identifier, *qualified* with the dictionary name. If no such key exists in the dictionary, an error will be raised

Such unqualified, single-quoted symbol keys are also called *fields*

For example:
```
# single key 'my_field (an unqualified, single-quoted symbol)
# single value "my_value" (a string)
(def my_dict {('my_field "my_value)}

# obtain value of symbol key
# simply evaluate the matching key identifier,
# qualified with the dictionary identifier
my_dict.my_field # -> "my_value"

# updating value of symbol key
# returns a new dictionary with the updated key/value pair
(set my_dict.my_field 123) # -> {('my_field 123)}
```

The following functions are used to manipulate dictionaries (regardless of key type):
```
(dict_is_empty {})                      # -> 'true
(dict_is_empty {("my_key" 'my_value)})  # -> 'false

(dict_get_size {})                      # -> 0
(dict_get_size {("my_key" 'my_value)})  # -> 1

(dict_get_keys {})                      # -> []
(dict_get_keys {("my_key" 'my_value)})  # -> ["my_key"]

(dict_get "my_key" {})                      # -> (), i.e. absent
(dict_get "my_key" {("my_key" 'my_value)})  # -> 'my_value, i.e. present

(dict_put "my_key" 'my_value {})                      # -> {("my_key" 'my_value)}, i.e. adds key/value pair
(dict_put "my_key" 'my_value {("my_key" 123)})        # -> {("my_key" 'my_value)}, i.e. replaces key/value pair
(dict_put "my_key" 'my_value {("my_key" 'my_value)})  # -> {("my_key" 'my_value)}, i.e. no change

(dict_del "my_key" {})                # -> {}, i.e. no change
(dict_del "my_key" {("my_key" 123)})  # -> {}, i.e. removes key/value pair
```

# Functions

Here are some function literals:
```
(func () ())            # nullary, returns ()
(func (x) x)            # unary, returns its arg
(func (n m) (add n m))  # binary, sums its args
# etc
(func args args)        # variadic, returns its args
```

Here are some useful functions:
```
(def id (func (x) x))
(def apply (func (f x) (f x)))
(def flip (func (f x y) (f y x)))
(def compose (func (f g) (func (x) (f (g x)))))
```

A function's argument list can be introspected like this:
```
(func_args id)      # -> ['id]
(func_args apply)   # -> ['f 'x]
(func_args flip)    # -> ['f 'x 'y]
(func_args compose) # -> ['f 'g]
(func_args list)    # -> 'args, i.e. variadic
```

All functions have type `'func`, which corresponds to the `func` function used in function literals

Note that, by default, function arguments are evaluated (left to right) before being passed to the function proper. We will see in the next section how to modify this behaviour

## Argument-passing behaviours

A function can be annotated with an argument-passing behaviour symbol. This symbol instructs the interpreter on what to do with the function arguments right before passing them to the function proper. They must be one of the following symbols:
- `'eval`: evaluate the arguments (left to right). This is the default behaviour
- `'quote`: convert the arguments to code (identifiers to symbols, etc)
- `'unquote`: the reverse of `'quote`: convert (and evaluate) the arguments from code (left to right)
- `'deep_quote`: like `'quote`, but replace an identifier with its associated value beforehand
- `'deep_unquote`: like `'unquote`, but replace an identifier with its associated value beforehand

In a function literal, this symbol must appear immediately before the function argument declaration

Here are some examples of functions using such symbols:
```
# evaluate the arguments left to right
# the following two functions are equivalent
(func (x) x)
(func 'eval (x) x)

# quote the arguments
(func 'quote (x) x)

# unquote (and evaluate) the arguments left to right
(func 'unquote (x) x)

# deep-quote the arguments
(func 'deep_quote (x) x)

# deep-unquote (and evaluate) the arguments left to right
(func 'deep_unquote (x) x)
```

A function's argument-passing behaviour symbol can be accessed like this:
```
(def func_eval          (func 'eval         (x) x))
(def func_quote         (func 'quote        (x) x))
(def func_unquote       (func 'unquote      (x) x))
(def func_deep_quote    (func 'deep_quote   (x) x))
(def func_deep_unquote  (func 'deep_unquote (x) x))

(func_arg_pass func_eval)         # -> 'eval
(func_arg_pass func_quote)        # -> 'quote
(func_arg_pass func_unquote)      # -> 'unquote
(func_arg_pass func_deep_quote)   # -> 'deep_quote
(func_arg_pass func_deep_unquote) # -> 'deep_unquote
```

The following sections describe each of these symbols in more details

### Evaluating function arguments with `'eval`

This is the default argument-passing behaviour

Arguments are evaluated in two different ways:
- literals (ex: `()`, `0`, `""`, `'a`, `[]`, `{}`, `(func (x) x)`):
  - return the value represented by the literal
  - lists and dictionaries may need to evaluate their contents recursively

- identifiers (ex: `true`, `fib`):
  - return the value associated to the identifier. Ex:
    - given: `(def my_value "hello world")`
    - evaluating `my_value` returns `"hello world"`
  - an undefined identifier error may be raised if the identifier is not previously defined

Here is a detailed example:
```
# given:
(def my_func (func 'eval (x) x)) # eq. to (func (x) x)
(def n 123)

# (fac 5) and n are evaluated here to 120 and 123 resp.
(def my_value [ 0 "" 'a [] {} (fac 5) n ])

# when we evaluate my_value or the associated list literal:
(my_func my_value)
(my_func [ 0 "" 'a [] {} (fac 5) n ])

# both return the same thing:
# [ 0 "" 'a [] {} 120 123 ]
# notice how (fac 5) and n, previously evaluated, were unchanged
```

### Quoting function arguments with `'quote`

See section *Quoting values* for mode details on how to quote values

Here is a detailed example:
```
# given:
(def my_func (func 'quote (x) x))
(def n 123)
(def my_value [ 0 "" 'a [] {} (fac 5) n ])

# when we quote identifier my_value:
(my_func my_value)

# then returns the matching symbol:
# 'my_value

# when, instead, we quote the associated list literal:
(my_func [ 0 "" 'a [] {} (fac 5) n ])

# then returns the literal, quoted:
# ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated here, but quoted
```

### Unquoting function arguments with `'unquote`

Unquoting is the reverse operation of quoting

See section *Unquoting values* for mode details on how to unquote values

Here is a detailed example:
```
# given:
(def my_func (func 'unquote (x) x))
(def n 123)
(def my_code ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n])

# when we unquote symbol 'my_code:
(my_func 'my_code)

# then returns the identifier my_code, evaluated:
# ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated yet

# when, instead, we unquote the associated list literal:
(my_func ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n])

# then returns the literal, unquoted:
# [0 "" 'a [] {} 120 123]
# notice how (fac 5) and n were evaluated here to 120 and 123, respectively
```

### Deep-quoting function arguments with `'deep_quote`

Deep-quoting function arguments is similar to simply quoting them, except for one main difference

When quoting an identifier produces a symbol, deep-quoting an identifier will first evaluate the identifier, and then quote the resulting value. Deep-quoting a literal is the same as quoting it normally

See section *Quoting values* for mode details on how to quote values

Here is a detailed example:
```
# given:
(def my_func (func 'deep_quoted (x) x))
(def n 123)

# (fac 5) and n are evaluated here to 120 and 123 resp.
(def my_value [ 0 "" 'a [] {} (fac 5) n ])

# when we deep-quote identifier my_value:
(my_func my_value)

# then returns the associated value, quoted:
# ['list 0 "" ''a ['list] ['dict] 120 123]
# notice how (fac 5) and n, previously evaluated, were quoted here as themselves

# when, instead, we deep-quote the associated list literal:
(my_func [ 0 "" 'a [] {} (fac 5) n])

# then returns the literal, quoted:
# ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated here, but quoted
```

### Deep-unquoting function arguments with `'unquote`

Deep-unquoting function arguments is similar to simply unquoting them, except for one main difference

While normal-unquoting an identifier is not allowed, deep-unquoting an identifier will first evaluate the identifier, and then unquote the resulting value. Deep-unquoting a literal is the same as unquoting it normally

See section *Unquoting values* for mode details on how to unquote values

Here is a detailed example:
```
# given:
(def my_func (func 'deep_unquoted (x) x))
(def n 123)
(def my_code ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n])

# when we deep-unquote symbol 'my_code:
(my_func 'my_code)

# then returns the identifier my_code, evaluated (not unquoted):
# ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated
# this is the same behaviour as normal-unquoting a symbol literal

# when, instead, we deep-unquote identifier my_code or the associated list literal:
(my_func my_code)
(my_func ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n])

# then returns the literal, unquoted:
# [0 "" 'a [] {} 120 123]
# notice how (fac 5) and n were evaluated here to 120 and 123, respectively
```

### Quoting values

This operation is the reverse of unquoting values (see below)

Quoting any value will only produce results of the following types:
- numbers
- strings
- symbols
- lists, containing zero or more recursive sub-results (i.e. numbers, strings, symbols, lists)

Quoting adheres to the following rules:
- quoting an identifier produces the matching single-quoted symbol. Ex:
  - quoting `true` produces `'true`

- quoting a number literal produces the number itself. Ex:
  - quoting `123` produces `123`

- quoting a string literal produces the string itself. Ex:
  - quoting `"abc"` produces `"abc"`

- quoting a symbol literal produces a new symbol with one more leading quote. Ex:
  - quoting `'true` (one quote) produces `''true` (two quotes)
  - quoting `''true` (two quotes) produces `'''true` (three quotes)

- quoting a list literal instead quotes the corresponding form. Ex:
  - quoting `[() 0 "" 'a [] {} (func () ()) (fac 5)]` instead quotes:
    - `(list () 0 "" 'a [] {} (func () ()) (fac 5))`
    - see below on how to quote forms

- quoting a dictionary literal instead quotes the corresponding form. Ex:
  - quoting `{(() 0) ("" 'a) ([] {}) ((func () ()) (fac 5))}` instead quotes:
    - `(dict (() 0) ("" 'a) ([] {}) ((func () ()) (fac 5)))`
    - see below on how to quote forms

- quoting a form produces a list of each elements, quoted recursively. Parentheses are replaced with square brackets during this process. Ex:
  - quoting `()` (i.e. an empty form, the unit type) produces the empty list `[]`

  - quoting `(func (x) x)` produces `['func ['x] 'x]`

  - quoting `(func 'eval (x) x)` produces:
    - `['func ''eval ['x] 'x]`

  - quoting `(list () 0 "" 'a [] {} (func () ()) (fac 5))` produces:
    - `['list [] 0 "" ''a ['list] ['dict] ['func [] []] ['fac 5]]`
    - notice how `(fac 5)` is not evaluated by this process

  - quoting `(dict (() 0) ("" 'a) ([] {}) ((func () ()) (fac 5)))` produces:
    - `['dict [[] 0] ["" ''a] [['list] ['dict]] [['func [] []] ['fac 5]]]`
    - notice how `(fac 5)` is not evaluated by this process

### Unquoting values

This operation is the reverse of quoting values (see above)

Unquoting only works on the types produced by quoting, i.e. numbers, strings, symbols and lists

Unquoting adheres to the following rules:
- unquoting a number literal produces the number itself. Ex:
  - quoting `123` produces `123`

- unquoting a string literal produces the string itself. Ex:
  - quoting `"abc"` produces `"abc"`

- quoting a symbol literal with at least two leading quotes produces a new symbol with one less leading quote. Ex:
  - quoting `'''true` (three quotes) produces `''true` (two quotes)
  - quoting `''true` (two quote) produces `'true` (one quotes)

- unquoting a single-quoted symbol literal evaluates the corresponding identifier (i.e. without the leading quote). An error may be raised if this identifier is not previously defined. Ex:
  - unquoting `'true` evaluates the identifier `true`, which produces `'true`
  - unquoting `'id` evaluates the identifier `id`, which produces `(func (x) x)`

- unquoting a list literal produces an unevaluated form with each elements unquoted recursively. Square brackets are replaced with parentheses during this process. The form is then evaluated according to the function's specified argument-passing behaviour. Ex: 
  - unquoting `[]` produces the unit type `()` (i.e. the empty form)
  - unquoting `['func ['x] 'x]` produces:
    - `(func (x) x)`
    - which evaluates to the corresponding function, i.e. the `id` function

  - unquoting `['func ''eval ['x] 'x]` produces:
    - `(func 'eval (x) x)`
    - which evaluates to the corresponding function, i.e. the `id` function

  - unquoting `['list [] 0 "" ''a ['list] ['dict] ['func [] []] ['fac 5]]` produces:
    - `(list () 0 "" 'a [] {} (func () ()) (fac 5))`
    - which evaluates to `[() 0 "" 'a [] {} (func () ()) 120]`
    - notice how `(fac 5)` is evaluated by this process

  - unquoting `['dict [[] 0] ["" ''a] [['list] ['dict]] [['func [] []] ['fac 5]]]` produces:
    - `(dict (() 0) ("" 'a) ([] {}) ((func () ()) (fac 5)))`
    - which evaluates to `{(() 0) ("" 'a) ([] {}) ((func () ()) 120)}`
    - notice how `(fac 5)` is evaluated by this process

## Impure functions

A function is said to be *impure* when it receives and returns an additional context argument. This context argument is used to perform impure actions, like:
- performing I/O
- getting other module contexts
- aborting and resuming evaluation

An impure function literal differs from a a normal function literal in the following ways:
- an argument-passing behaviour symbol MUST appear immediately before the function argument declaration
- an additional context argument (with arbitrary name) must appear immediately before the argument-passing behaviour symbol
- the function must return a size-2 list containing (in order):
  - a context value, derived from the context argument (and possibly modified)
  - the function's otherwise normal return value

Here are some impure function literals that do not perform any impure actions:
```
(func ctx 'eval ()    [ctx ()])         # nullary func, returns ()
(func ctx 'eval (x)   [ctx x])          # unary, returns its arg
(func ctx 'eval (n m) [ctx (add n m)])  # binary, sums its args
# etc
(func ctx 'eval args  [ctx args])       # variadic, returns its args
```

Note that all examples used identifier `ctx` to refer to the additional context argument, but that is only a convention

Later in this document, we will encounter impure functions that modify the additional context argument

## Functions and contexts

Any function has access to an implicit context. Impure functions have access to an additional, explicit context

A context is a dictionary made up of unqualified, single-quoted symbol keys each associated to a given value. Such values can be of any type. This context dictionary is initialized at the beginning of a module evaluation, and can be used and modified by impure functions

A context is said to be implicit when its keys can be referenced without being qualified by the context name, as simple identifiers. Conversely, a context is said to be explicit when its keys must be qualified with the context name, resulting in a qualified identifier.

Another way to think about implicit contexts is like this:
- pretend that there exists a context named `this` or `self` (similar to some object-oriented languages)
- all unqualified identifiers are implicitely qualified with this `this` or `self` identifier

For example:
```
# add and get key 'zero in implicit context
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

By default, a function only has access to its implicit context, called the *function implicit context*

This *function implicit context* is the context of all identifiers (arguments, lambda closure, etc) that are defined when the function is created (i.e. using a function literal). Only those identifiers will be available to the function when it is invoked

A function's implicit context can be introspected like this:
```
# given
(def add (func (x y) "<internal + on x and y>"))
(def mult (func (x y) "<internal * on x and y>"))

# define custom function
(def my_add (func (n) (add n 1)))

# when getting the implicit context
(func_get_ctx my_add)
# -> {('add (func (x y) "<internal + on x and y>"))}

# setting implicit context
(def my_mult (func_set_ctx {('add mult)} my_add))
```

The `def` operation seen up until now will also add the defined name to a function's own implicit context, thus allowing simple recursion to work as expected

For mutually-recursive functions, one can devise an operation `mutual` that takes a number of functions, and will add all of them in each of the function implicit contexts

### The function explicit context

An impure function receives an additional context argument, called the *function explicit context*

This *function explicit context* is the context of all definitions available to the *caller* of the function. For this reason, it is also sometimes called *caller context*. Since it is an explicit context, all of its keys must be qualified with the context name

Note that when using an explicit context, the chosen identifier (ex: `'ctx`) will be added to the function's implicit context, unless such an identifier is already defined in the function's implicit context, in which case an error is raised

Here is an example of receiving (but not modifying) the explicit context:
```
def my_func (func ctx 'eval (x y z)
  (print ctx.zero)
  (print ctx.hello)

  [ ctx         # unchanged
    (add x y z) # return value
  ]
))

# set-up the caller context
(def zero 0)
(def hello "hello")

# invoke function
(func 1 2 3)
# prints: 0
# prints: "hello"
# returns: 6

# the caller context was not modified
(print zero)  # prints: 0
(print hello) # prints: "hello"
```

Here is an example of performing an impure action by modifying the explicit context:
```
def my_func (func ctx 'eval (x y z)
  (print ctx.n)
  (print ctx.hello)

  # updated explicit context
  # contexts are just dictionaries
  (def out_ctx (set ctx.n (add ctx.n 1)))

  [ out_ctx     # updated
    (add x y z) # return value
  ]
))

# set-up the caller context
(def n 0)
(def hello "hello")

# invoke function
(my_func 1 2 3)
# prints: 0
# prints: "hello"
# returns: 6

# caller context has changed
(print n) # prints: 1
```

A function's explicit context argument can be accessed like this:
```
(def func_pure    (func (x) x))
(def func_impure  (func ctx 'eval (x) [ctx x]))

(func_explicit_ctx func_pure)   # -> (), i.e. no explicit context
(func_explicit_ctx func_impure) # -> 'ctx
```

## Other uses for the explicit context

Beyond simply manipulating the caller context, the *function explicit context* can also be used to do the following:
- performing I/O
- getting other module contexts
- aborting and resuming evaluation

They are implemented using *volatile context fields*, which are context fields with unspecified behaviours outside the ones described in the following sections

### Performing I/O

The *function explicit context* can be used to open, read and write files. This allows the implementation of features like:
- read and write to the console
- read and write to disk
- send and receive data over the network

Assume an impure function `F`. Once `F` finishes its invocation, the interpreter will search the modified explicit context for the key `'__REQUEST_IO__`. If found, the interpreter will do the following:
- analyze the associated value for the type of I/O to perform. Ex:
  - for writing `"Hello World"` to the console, the value could be: `['std_out, "Hello World!"]`
  - for reading from the console, the value could be: `'std_in`
  - etc

- remove this key from the explicit context
- perform the requested I/O operation, putting key `'__RESPONSE_IO__` with the result in the explicit context
  - this key can afterwards be inspected by other impure functions
  - it is suggested that this key be deleted from the explicit context to avoid memory leaks

Here is an example of implementing a prompt functionality:
```
# prompt 1/3: request the writing of the prompt output string
(def prompt1 (func ctx 'eval (str)
  [ (put ctx '__REQUEST_IO__  ['std_out str])
    ()
  ]
))

# prompt 2/3: request the reading of the user input string
(def prompt2 (func ctx 'eval ()
  # ignore 'prompt1 response in ctx.__RESPONSE_IO__
  [ (put ctx '__REQUEST_IO__ 'std_in)
    ()
  ]
))

# prompt 3/3: process user string response
(def prompt3 (func 'eval ()
  [ (del ctx.__RESPONSE_IO__)
    ctx.__RESPONSE_IO__
  ]
))

# putting it all together:

(prompt1 "Enter your name: ")
# prints: Enter your name: 

(prompt2)
# waits for user input
# user inputs their name: John

(prompt3)
# returns: "John"
```

### Getting other module contexts

The *function explicit context* can be used to get other module definition contexts (i.e. the final context after a module has finished evaluating). This allows the implementation of features like:
- importing all or a subset of another module's definition
- aliasing, qualifying or hiding a subset of another module's definition

Assume an impure function `F`. Once `F` finishes its invocation, the interpreter will search the resulting explicit context for the key `'__REQUEST_MODULE_CTX__`. If found with a fully-qualified, single-quoted symbol `S`, the interpreter will do the following:
- remove this key from the explicit context
- search the module directory for the module `M` matching symbol `S`
- if module `M` is already evaluated, denote its final context as `C`
- if module `M` is not evaluated, evaluate it now, denoting its final context as `C`
  - see below for full module evaluation order
- put a key `'__RESPONSE_MODULE_CTX__` with value `C` in the explicit context
  - this key can afterwards be inspected by other impure functions
  - it is suggested that this key be deleted from the explicit context to avoid memory leaks

Here is an example of implementing an import functionality:
```
# import 1/2: send request
(def import1 (func ctx 'eval (module)
  [ (put ctx '__REQUEST_MODULE_CTX__  module)
    ()
  ]
))

# import 2/2: process response
(def import2 (func ctx 'eval ()
  [ (del ctx.__RESPONSE_MODULE_CTX__)
    ctx.__RESPONSE_MODULE_CTX__
  ]
))

(import1 'data.list)
(import2)
# returns: context for module Data.List
```

### Aborting and resuming evaluation

The *function explicit context* can be used to abort and resume evaluation. This allows the implementation of features like:
- error-handling with try/recover/finally
- forcefully return a value, thus bypassing API restrictions

The interpreter will keep a stack `S` of entries containing:
- an evaluation state (internal, non-representable value)
- a context (a dictionary)
- a resume evaluation handler (a unary function)

Assume an impure function `F`. Once `F` finishes its invocation, the interpreter will search the resulting explicit context for either of the following keys:
- `'__PUSH_RESUME_EVAL_HANDLER__`: if found with unary `'func` value, the interpreter will do the following:
  - remove this key from the explicit context
  - take a copy of its current evaluation state
  - take a copy of the current context
  - push a new entry on top of stack `S`

- `'__POP_RESUME_EVAL_HANDLER__`: if found, the interpreter will do the following:
  - remove this key from the explicit context
  - if stack `S` is empty, do nothing
  - otherwise, pop the topmost entry from stack `S`

- `__ABORT_EVAL_WITH__`: if found with value `V`, the interpreter will do the following:
  - remove this key from the explicit context
  - if stack `S` is empty, halt with message derived from `V`
  - otherwise, read (but do not pop) the top of stack `S` for an entry:
    - invoke the entry's resume evaluation handler (a unary function), using the entry's evaluation state and context

Note: It is the responsibility of the programmer to pop the resume evaluation handler from stack `S` when it is no longer needed:
- once aborted evaluation should not be resumed
- once aborted evaluation has been resumed

Here is an example of aborting and resuming evaluation:
```
# this function will push a resume evaluation handler
(def my_push (func ctx 'eval (handler)
  [ (put ctx '__PUSH_RESUME_EVAL_HANDLER__ handler)
    ()
  ]
))

# this function will pop a resume evaluation handler
(def my_pop (func ctx 'eval ()
  [ (put ctx '__POP_RESUME_EVAL_HANDLER__ ())
    ()
  ]
))

# this function will abort evaluation
(def my_abort (func ctx 'eval (abort_with)
  [ (put ctx '__ABORT_EVAL_WITH__  abort_with)
    ()
  ]
))

# putting it all together

# define handler
(def my_handler (func (x)
  (my_pop) # pop handler
  (print x)
))

# push handler
(my_push my_handler)

# forgotten after evaluation aborts
(def my_other_value 1)

# abort evaluation
(my_abort "hello crash world!")
# invokes my_handler
# prints: "hello crash world!"
```

# Modules

Source code can be split in multiple files. Each file corresponds to a module, and the path of a file corresponds to the module identifier. In particular, the relative path from the source root of any given file corresponds to the fully-qualified identifier of the corresponding module. For example, the following file structure and associated module identifiers:
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

    2. if `e` access the context of another module `m`:
        1. let `modules` now be: `(put modules module ctx)`
        2. call `(evaluate modules m)`
        3. finish evaluating `e`

6. when `module` is fully evaluated with a final context `ctx`:
    1. let `modules` now be: `(put modules module ctx)`

This algorithm ensures that:
  - required (and only required) modules are ever compiled and evaluated
  - circular dependencies are allowed and will not cause an infinite recursion
    - they MAY cause undefined identifier errors, ex:
      - module `A` imports module `B`, in which module `B` imports module `A`
      - mdoule `B` will not have access to identifiers of `A` defined after its module `A`'s `(import B)` statement
      - this is because evaluation of `A` is paused until module `B` finishes evaluating

    - this can be fixed by doing one of the following:
      - creating a `friend zone` in A and B, where code required for both to evaluate correctly comes before the import statement of the other
      - moving dependencies to a new module

# TODOs

The preceding only touches upon built-in primitives of the language. The goal is that more advanced features can be implemented in the language itself using those primitives

## Error Handling

(try (try_statement)
  (recover e (recover_statement)) # optional if 'finally is present
  (finally (finally statement))   # optional if 'recover is present
)

Uses `'__ABORT_EVAL_WITH__` under the hood

## Modules and imports

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

### Exports and private identifiers

By default, every definition in a module `F` will be exported (i.e. made available to all other modules importing `F`). This includes identifiers defined in the module itself, as well as those imported from other modules (transitive imports, or re-exports)

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

### The `base` module

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

## Defining custom structs

A struct is just a dictionary with a special field `'__STRUCT__` that contains the type of the struct

Redefine `'typeof` to check this field

Operations like `'defstruct` and `'struct` will read and update context key `'__STRUCTS__`, which contains the types, fields, etc, of structs that are defined in the current context

## Protocols

Operations like `'protocol` and `'impl` will read and update context key `'__PROTOCOLS__`, which contains the types, functions, implementations of protocols that are defined in the current context. Protocol (i.e. abstract) functions will check this to find the appropriate implementation
