# radicle

> because 'tis the Origin of the Root... The Radicle is likewise called  the
seminal Root.
> - Vallemont, *Curiosities of nature and art in husbandry and gardening* (1707)

A LISP in the spirit of [Black](http://pllab.is.ocha.ac.jp/~asai/Black/) and
other colors.

## Installation

Install stack. Then:
```
stack build
```

## How to run tests

```
stack test
```

## How to run executables

Executing a .rad script and running a REPL are equivalent. Some scripts (in particular
`rad/repl.rad`) simply *define* a REPL.

Running your own client will likely have more functionality than the in-browser demo at radicle.xyz.

```
stack exec client -- --config rad/repl.rad --url radicle.xyz
```

## The language

The language currently lacking complete documentation, however a good place to start is the rad/ directory, which contains a number of radicle files.

Variables defined there are usually documented. This means you can type `(doc
'ident)` to retrieve documentation for the identifier `ident`.
