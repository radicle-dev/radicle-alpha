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

Running scripts and a REPL are the same thing - some scripts (in particular
`rad/repl.rad`) just *define* a REPL.

You should probably run the client against radicle.xyz for fuller
functionality.

```
stack exec radicle-client -- --config rad/repl.rad --url radicle.xyz
```

## The language

The language currently lacking good documentation. A good place to get a sense
for it is in the rad/ directory, which contains a number of radicle files.

Variables defined there are usually documented. This means you can type `(doc
'ident)` to retrieve documentation for the identifier `ident`.
