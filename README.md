# Comments

The `#` character starts a single-comment, which ends at the end of the current line:
```
# This is a single-comment
# This is another single-line comment
```

There are no multi-line comments

# Forms

Parentheses ( `(` and `)` ) denote forms (i.e. a function invocation). For example:
```
()        # is the empty form, or unit type (see below)
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

The `type_of` function can be used to obtain the type of a value:
```
(type_of ())         # -> ()
(type_of any_num)    # -> 'num
(type_of any_str)    # -> 'str
(type_of any_symb)   # -> 'symb
(type_of any_list)   # -> 'list
(type_of any_dict)   # -> 'dict
(type_of any_func)   # -> 'func
```

# The unit type `()`

Also know as the empty form. Has only one value: `()`, which is also its own type

Used to represent the absence of any meaningful value. Has no meaningful functions associated with it

# Numbers

Numbers can be ints (53-bit precision) or floats (64-bit precision)

Integer literals (positive or negative):
```
0 1 2 3 4 5 10 123
-1 -2 -3 -4 -5 -10 -123
```

Floating-point number literals:
```
0.1 1.1 2.2 10.01 123.123
-1.1 -2.2 -3.3 -10.01 -123.123
1.2e12 -1.2E-12
```

All numbers have type `'num`, which corresponds to the following function `num`:
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
"àéèïîôöù"          # unicode printable characters
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
(size "")    # -> 0
(size "abc") # -> 3

(split "" "abc")    # -> ["a" "b" "c"]
(split " " "a b c") # -> ["a" "b" "c"]

(join "" "a" "b" "c")   # -> "abc"
(join " " "a" "b" "c")  # -> "a b c"
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

The number of quotes of a symbol can be obtained like this:
```
(nbr_quotes 'foo)           # -> 1
(nbr_quotes ''bar)          # -> 2
(nbr_quotes '''foo.bar.baz) # -> 3
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
# function 'def associates an identifier to a value
(def my_value "hello world")
```
We can say that identifier `my_value` represents the value `"hello world"`, and its matching symbol looks like `'my_value`. When evaluating identifier `my_value`, it will produce value `"hello world"`

Here are some identifiers, matching the previously mentioned symbols (i.e. without quotes):
```
a b c
abc
_foo BAR99 B4Z
true false
```

Any identifier can be converted into a symbol by *quoting* it, i.e. adding one or more leading quote

Conversely, any symbol can be converted into an identifier by *unquoting* it, i.e. removing one or more leading quote and evaluating the result. Keep in mind that unquoting a symbol may lead to an undefined identifier error

Note that a symbol by itself is not required to correspond to a previously defined identifier. Only the unquoting process requires that relationship between symbols and identifiers. For example, symbol `'foo` is a perfectly valid symbol and can be freely passed around, even places where identifier `foo` is not defined. Only when unquoting `'foo` does the identifier `foo` need to be defined

We will see later on in this document how to programmatically quote and unquote identifiers and symbols

## Booleans

Boolean values (`false` and `true`) are implemented using symbols like this:
```
(def false 'false)
(def true 'true)
```

Which means that boolean values are associated identifiers that, when evaluated, return themselves. In other words, it means that unquoting a boolean symbol returns itself

Boolean operations (`not`, `or`, `and`) are actually applicable to all types. They operate on the *boolish* versions of their arguments, which can be obtained according to the following rules:

- symbol `'false` is `'false`
- symbol `'true` is `'true`

- the following values are *falsish*:
  - `()`: the unit type
  - `0`: the number zero
  - `""`: the empty string
  - `[]`: the empty list
  - `{}`: the empty dictionary

- any other value is *truish*. For example:
  - `1`: any non-zero number
  - `"abc"`: any non-empty string
  - `'xyz`: any non-`'true` or non-`'false` symbol
  - `[0]`: any non-empty list, regardless of contents
  - `{[0 0]}`: any non-empty dictionary, regardless of contents
  - `(func (x) x)`: any function whatsoever

The following truth table applies for the `not` operation:

| x         | (not x)
|-----------|---------
| `'false`  | `'true`
| *falsish* | `'true`
| *truish*  | `'false`
| `'true`   | `'false`

The following truth table applies for the `or` operation:

| x         | y        | (or x y)
|-----------|----------|---------
| `'true`   | *        | x (i.e. `'true` wins against anything)
| *truish*  | `true`   | y (i.e. `'true` wins against anything)
| *truish*  | *        | x (i.e. *truish* wins against anything not `'true`)
| *falsish* | `'false` | x (i.e. *falsish* only wins against `'false`)
| *falsish* | *        | y (i.e. *falsish* only wins against `'false`)
| `'false`  | *        | y (i.e. `'false` wins against nothing)

The following truth table applies for the `and` operation:

| x         | y        | (and x y)
|-----------|----------|---------
| `'false`  | *        | x (i.e. `'false` wins against anything)
| *falsish* | `false`  | y (i.e. `'false` wins against anything)
| *falsish* | *        | x (i.e. *falsish* wins against anything not `'false`)
| *truish*  | `'true`  | x (i.e. *truish* only wins against `'true`)
| *truish*  | *        | y (i.e. *truish* only wins against `'true`)
| `'true`   | *        | y (i.e. `'true` wins against nothing)

As you can see, the boolish truch tables for `or` and `and` are somewhat the opposite of each other. Also, remember that note that `or` and `and` will always evaluate *both* their arguments (i.e. they do not short-circuit as in other programming languages)

Additionally, the following functions work on all data types and return boolean values:
```
(eq x y)  # -> 'true or 'false. Whether x and y are equal
# values of different types are automatically different

(lt x y)  # -> 'true or 'false. Whether x is less than y
# values of different types are ordered as such:
# () < any_num < any_str < any_symb < any_list < any_dict < any_func
# values of the same type are ordered reasonably
```

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

The square bracket notation (i.e. using `[` and `]` ) is useful for cleaning-up the source code, while the `list` function is useful to represent a list as a form, which simplifies quoting and unquoting lists

All lists have type `'list`, which corresponds to the previously mentioned `list` function

The following functions are used to manipulate lists:
```
(is_empty [])      # -> 'true
(is_empty [1 2 3]) # -> 'false

(size [])      # -> 0
(size [1 2 3]) # -> 3

(cons 1 [])    # -> [1]
(cons 1 [2 3]) # -> [1 2 3]

(head [])      # -> error
(head [1 2 3]) # -> 1

(tail [])      # -> error
(tail [1 2 3]) # -> [2 3]
```

# Dictionaries

Dictionaries are immutable and heterogenous containers of key/value pairs (i.e. size-2 lists). Values of any types can be keys in dictionaries, because there is a strict ordering across all data types, regardless of type (yes, even functions)

Here are some dictionary literals:
```
{}
{ [1 2] [3 4] }
{
    [() 0]
    [0 ""]
    ["" 'dict]
    ['dict []]
    [[] {}]
    [{} (func () ())]
    [(func () ()) ()]
}
```

The preceding examples can also be expressed using the `dict` function:
```
(dict)
(dict [1 2] [3 4] )
(dict
    [() 0]
    [0 ""]
    ["" 'dict]
    ['dict []]
    [[] {}]
    [{} (func () ())]
    [(func () ()) ()]
)
```

Note that dictionary entries are just lists that must have exactly 2 elements, otherwise an error is thrown. Identifiers associated to size-2 lists can also be used in dictionary literals:
```
(def p1 [() 0])
(def p2 [0 ""])
(def p3 ["" 'dict])
(def p4 ['dict []])
(def p5 [[] {}])
(def p6 [{} (func () ())])
(def p7 [(func () ()) ()])
{ p1 p2 p3 p4 p5 p6 p7 }
(dict p1 p2 p3 p4 p5 p6 p7)
```

The curly brace notation (i.e. using `{` and `}` ) is useful for cleaning-up the source code, while the `dict` function is useful to represent a dictionary as a form, which simplifies quoting and unquoting dictionaries

All dictionaries have type `'dict`, which corresponds to the previously mentioned `dict` function

The following functions are used to manipulate dictionaries:
```
(is_empty {})                      # -> 'true
(is_empty {["my_key" 'my_value]})  # -> 'false

(size {})                      # -> 0
(size {["my_key" 'my_value]})  # -> 1

(keys {})                      # -> []
(keys {["my_key" 'my_value]})  # -> ["my_key"]

(assocs {})                      # -> []
(assocs {["my_key" 'my_value]})  # -> [["my_key", 'my_value]]

(contains {} "my_key")                      # -> 'false
(contains {["my_key" 'my_value]} "my_key")  # -> 'true

(get {} "my_key")                      # -> (), i.e. absent
(get {["my_key" 'my_value]} "my_key")  # -> 'my_value, i.e. present

(put 'my_value {} "my_key")                      # -> {["my_key" 'my_value]}, i.e. adds key/value pair
(put 'my_value {["my_key" 123]} "my_key")        # -> {["my_key" 'my_value]}, i.e. replaces key/value pair
(put 'my_value {["my_key" 'my_value]} "my_key")  # -> {["my_key" 'my_value]}, i.e. no change

(del {} "my_key")                # -> {}, i.e. no change
(del {["my_key" 123]} "my_key")  # -> {}, i.e. removes key/value pair
```

# Functions

Here are some function literals:
```
(func () ())            # nullary, returns (), i.e. the unit type
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
(get_args id)      # -> ['id]
(get_args apply)   # -> ['f 'x]
(get_args flip)    # -> ['f 'x 'y]
(get_args compose) # -> ['f 'g]
(get_args list)    # -> 'args, i.e. variadic
```

All functions have type `'func`, which corresponds to the function `func` used in function literals

Note that, by default, function arguments are evaluated (left to right) before being passed to the function proper. We will see in the next section how to modify this behaviour

## Argument-passing behaviours

A function can be annotated with an argument-passing behaviour symbol. This symbol instructs the interpreter on what to do with the function arguments right before passing them to the function proper. They must be one of the following symbols:
- `'eval`: evaluate the arguments (left to right). This is the default behaviour
- `'quote`: convert the arguments to code (identifiers to symbols, quoting literals, etc)
- `'unquote`: the reverse of `'quote`: convert (and evaluate) the arguments from code (left to right)
- `'deep_quote`: like `'quote`, but replace an identifier with its associated value beforehand
- `'deep_unquote`: like `'unquote`, but replace an identifier with its associated value beforehand

In a function literal, if present, this symbol must appear in a separate form, which MUST appear immediately before the function argument declaration

Here are some examples of functions using such symbols:
```
# evaluate the arguments left to right
# the following two literals are equivalent
(func (x) x)
(func ('eval) (x) x)

# quote the arguments
(func ('quote) (x) x)

# unquote (and evaluate) the arguments left to right
(func ('unquote) (x) x)

# deep-quote the arguments
(func ('deep_quote) (x) x)

# deep-unquote (and evaluate) the arguments left to right
(func ('deep_unquote) (x) x)
```

A function's argument-passing behaviour symbol can be accessed like this:
```
(def func_eval          (func ('eval)         (x) x))
(def func_quote         (func ('quote)        (x) x))
(def func_unquote       (func ('unquote)      (x) x))
(def func_deep_quote    (func ('deep_quote)   (x) x))
(def func_deep_unquote  (func ('deep_unquote) (x) x))

(get_arg_pass func_eval)         # -> 'eval
(get_arg_pass func_quote)        # -> 'quote
(get_arg_pass func_unquote)      # -> 'unquote
(get_arg_pass func_deep_quote)   # -> 'deep_quote
(get_arg_pass func_deep_unquote) # -> 'deep_unquote
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
(def my_func (func ('eval) (x) x)) # eq. to (func (x) x)
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
(def my_func (func ('quote) (x) x))
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
(def my_func (func ('unquote) (x) x))
(def n 123)
(def my_code ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n])

# when we unquote symbol 'my_code:
(my_func 'my_code)

# then returns the identifier my_code, evaluated (not unquoted):
# ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated yet

# when, instead, we unquote the associated list literal:
(my_func ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n])

# then returns the literal, unquoted (and evaluated):
# [0 "" 'a [] {} 120 123]
# notice how (fac 5) and n were evaluated here to 120 and 123, respectively
```

### Deep-quoting function arguments with `'deep_quote`

Deep-quoting function arguments is similar to simply quoting them, except for one main difference

While normal-quoting an identifier produces a symbol, deep-quoting an identifier will first evaluate the identifier, and then normal-quote the resulting value. Deep-quoting a literal is the same as normal-quoting it

See section *Quoting values* for mode details on how to quote values

Here is a detailed example:
```
# given:
(def my_func (func ('deep_quoted) (x) x))
(def n 123)

# (fac 5) and n are evaluated here to 120 and 123 resp.
(def my_value [ 0 "" 'a [] {} (fac 5) n ])

# when we deep-quote identifier my_value:
(my_func my_value)

# then returns the associated value, quoted:
# ['list 0 "" ''a ['list] ['dict] 120 123]
# notice how (fac 5) and n, previously evaluated, were quoted here as themselves (numbers)

# when, instead, we deep-quote the associated list literal:
(my_func [ 0 "" 'a [] {} (fac 5) n ])

# then returns the literal, quoted:
# ['list 0 "" ''a ['list] ['dict] ['fac 5] 'n]
# notice how (fac 5) and n were not evaluated here, but quoted
```

### Deep-unquoting function arguments with `'unquote`

Deep-unquoting function arguments is similar to simply unquoting them, except for one main difference

While normal-unquoting an identifier is not allowed, deep-unquoting an identifier will first evaluate the identifier, and then normal-unquote the resulting value. Deep-unquoting a literal is the same as normal-unquoting it

See section *Unquoting values* for mode details on how to unquote values

Here is a detailed example:
```
# given:
(def my_func (func ('deep_unquoted) (x) x))
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
  - quoting `{[() 0] ["" 'a] [[] {}] [(func () ()) (fac 5)]}` instead quotes:
    - `(dict [() 0] ["" 'a] [[] {}] [(func () ()) (fac 5)])`
    - see below on how to quote forms

- quoting a form produces a list of each elements, quoted recursively. Parentheses are replaced with square brackets during this process. Ex:
  - quoting the empty form `()` (i.e. the unit type) produces the empty list `[]`

  - quoting `(func (x) x)` produces `['func ['x] 'x]`

  - quoting `(func 'eval (x) x)` produces:
    - `['func ''eval ['x] 'x]`

  - quoting `(list () 0 "" 'a [] {} (func () ()) (fac 5))` produces:
    - `['list [] 0 "" ''a ['list] ['dict] ['func [] []] ['fac 5]]`
    - notice how `(fac 5)` is not evaluated by this process

  - quoting `(dict [() 0] ["" 'a] [[] {}] [(func () ()) (fac 5)])` produces:
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
  - unquoting the empty list `[]` produces the empty form `()` (i.e. the unit type)
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
    - which evaluates to `{[() 0] ["" 'a] [[] {}] [(func () ()) 120]}`
    - notice how `(fac 5)` is evaluated by this process

## Functions and contexts

Any function has access to an implicit context. Impure functions (see below) have access to an additional, explicit context

A context is a dictionary made up of single-quoted symbol keys (ex: `'foo`, `'bar`) each associated to a given value. Such values can be of any type. This context dictionary is initialized at the beginning of a module evaluation, and can be used and modified by impure functions

For example:
```
# add and get key 'zero in implicit context
(def zero 0)
(print zero)

# ctx is an explicit context
(def ctx {['zero 0]})

# get key 'zero from explicit context
(print (get 'zero ctx))
```

### The function implicit context

By default, a function only has access to its implicit context, called the *function implicit context*

This *function implicit context* is the context of all identifiers (arguments, lambda closure, etc) that are defined when the function is created (i.e. using a function literal). For this reason, the *function implicit context* can also be called the function *definition* context. Only identifiers present in this implicit context can be accessed when the function is invoked

A function's implicit context can be accessed like this:
```
# given
(def add_plus_one (func (x y) (add x y 1)))
(def add_plus_one_logged (func (x y)
  # log operands
  (print "x:" x ", y: " y)

  # log result
  (def result (add x y 1))
  (print "result:" result)

  # return result
  result
))

# define custom function
(def double_plus_one (func (n) (add_plus_one n n)))

# when getting the implicit context
(get_impl_ctx double_plus_one)
# -> {['add (func (x y) (add x y 1))]}

# setting implicit context
(def double_plus_one_logged (set_impl_ctx double_plus_one {['add add_plus_one_logged]}))
```

Keep in mind that manipulating the implicit context can cause the resulting function to misbehave, for example if a key of the implicit context is removed or is replaced with an incompatible value

### Recursion and the implicit context

In order for recusrion to work, a function's implicit context needs to contain itself. For this reason, defining functions using `def` will not allow recursion, since `def` does not modify the defined values whatsoever. Therefore, recursive functions must be defined using `defun`, which only works on functions but will add the function's own name to its own implicit context, thus allowing simple recursion to work

For mutually-recursive functions, one can devise an operation `mutual` that takes a number of functions, and will add all of their names in the implicit contexts of each of them

## Impure functions

A function is said to be *impure* when it receives and returns an additional context argument, called the *function explicit context* (see next section). This context argument is used to perform impure actions, like changing definitions in the caller context.

An impure function literal differs from a a normal function literal in the following ways:
- an argument-passing behaviour symbol MUST appear in a separate form, which MUST appear immediately before the function argument declaration
- an additional context argument (with arbitrary name) must appear immediately after the argument-passing behaviour symbol, in the same form
- the function must return a size-2 list containing (in order):
  - a dictionary value, possibly derived from the context argument (and possibly modified)
  - the function's otherwise normal return value

Here are some impure function literals that do not perform any impure actions:
```
(func ('eval ctx) ()    [ctx ()])         # nullary func, returns ()
(func ('eval ctx) (x)   [ctx x])          # unary, returns its arg
(func ('eval ctx) (n m) [ctx (add n m)])  # binary, sums its args
# etc
(func ('eval ctx) args  [ctx args])       # variadic, returns its args
```

Note that all examples used identifier `ctx` to refer to the additional context argument, but that is only a convention

### The function explicit context

An impure function receives an additional context argument, called the *function explicit context*

This *function explicit context* is the context of all definitions available to the *caller* of the function. For this reason, the *function explicit context* can also be called the function *caller context*

Here is an example of receiving (but not modifying) the explicit context:
```
def my_func (func ('eval ctx) (x y z)
  (print (get 'zero ctx)
  (print (get 'hello ctx)

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
def my_func (func ('eval ctx) (x y z)
  (print (get ctx 'n))
  (print (get ctx 'hello))

  # updated explicit context
  # contexts are just dictionaries
  (def out_ctx (put ctx 'n (add ctx.n 1)))

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
(def func_impure  (func ('eval ctx) (x) [ctx x]))

(get_expl_ctx func_pure)   # -> (), i.e. no explicit context
(get_expl_ctx func_impure) # -> 'ctx
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
(def prompt1 (func ('eval ctx) (str)
  [ (put ctx '__REQUEST_IO__  ['std_out str])
    ()
  ]
))

# prompt 2/3: request the reading of the user input string
(def prompt2 (func ('eval ctx) ()
  # ignore 'prompt1 response in ctx.__RESPONSE_IO__
  [ (put ctx '__REQUEST_IO__ 'std_in)
    ()
  ]
))

# prompt 3/3: process user string response
(def prompt3 (func 'eval ()
  [ (del ctx '__RESPONSE_IO__)
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
- aliasing or hiding a subset of another module's definition

Assume an impure function `F`. Once `F` finishes its invocation, the interpreter will search the resulting explicit context for the key `'__REQUEST_MODULE_CTX__`. If found with a list of single-quoted symbol `S`, the interpreter will do the following:
- remove this key from the explicit context
- search the module directory for the module `M` matching symbols in `S`
- if module `M` is already evaluated, denote its final context as `C`
- if module `M` is not evaluated, evaluate it now, denoting its final context as `C`
  - see below for full module evaluation order
- put a key `'__RESPONSE_MODULE_CTX__` with value `C` in the explicit context
  - this key can afterwards be inspected by other impure functions
  - it is suggested that this key be deleted from the explicit context to avoid memory leaks

Here is an example of implementing an import functionality:
```
# import 1/2: send request
(def import1 (func ('quote ctx) module
  [ (put ctx '__REQUEST_MODULE_CTX__  module)
    ()
  ]
))

# import 2/2: process response
(def import2 (func ('eval ctx) ()
  [ (del ctx '__RESPONSE_MODULE_CTX__)
    (get ctx '__RESPONSE_MODULE_CTX__)
  ]
))

(import1 data list)
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
(def my_push (func ('eval ctx) (handler)
  [ (put ctx '__PUSH_RESUME_EVAL_HANDLER__ handler)
    ()
  ]
))

# this function will pop a resume evaluation handler
(def my_pop (func ('eval ctx) ()
  [ (put ctx '__POP_RESUME_EVAL_HANDLER__ ())
    ()
  ]
))

# this function will abort evaluation
(def my_abort (func ('eval ctx) (abort_with)
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

Source code can be split in multiple files. Each file corresponds to a module, and the path of a file corresponds to the module identifier. The following file structures demonstrates examples of module identifiers:
```
src/
  main.pz       # module ['main]
  Ast.Ast       # module ['data]
  data/
    set.pz      # module ['data 'set]
    list.pz     # module ['data 'list]
    list/
      lazy.pz   # module ['data 'list 'lazy]
```

## Module evaluation order

The program evaluation algorithm is as follows:
1. let `modules` be `{}`, such that:
  - each key is a list of a module's single-quoted symbol. Ex:
    - `['main]`
    - `['data 'list 'lazy]`

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
      - mdoule `B` will not have access to identifiers of `A` defined after its module `A`'s `import B` statement
      - this is because evaluation of `A` is paused until module `B` finishes evaluating

    - this can be fixed by doing one of the following:
      - creating a `friend zone` in A and B, where code required for both to evaluate correctly comes before the import statement of the other
      - moving dependencies to a new module

## The `builtin` module

There exists a module named `builtin`, that contains all functions which are implemented natively by the interpreter. Examples of such functions are:
- the `func` function itself
- functions `add`, `sub`, etc, for manipulating numbers
- functions `head`, `tail`, `const`, etc, for manipulating lists

Since these functions cannot be deep-unquoted into reasonable values, they are simply deep-unquoted as themselves. For example:
- deep-unquoting `func` produces `'func`
- deep-unquoting `add`, `sub`, etc, produces `'add`, `'sub`, etc
- deep-unquoting `head`, `tail`, `cons`, etc, produces `'head`, `'tail`, `'cons` etc

This preserves the property that `(compose deep_unquote deep_quote)` is equivalent to `id` for those functions as well

## The `base` module

There exists a module named `base`, of which all identifiers are automatically accessible to all modules. This module contains identifiers that are widely used and have no other dependencies than themselves. Examples of such functions are:
- functions `def`, `defun`, etc, for manipulating identifiers
- functions `if`, `cond`, etc, for control structures
- functions `list`, `dict`, etc