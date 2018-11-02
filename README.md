# radicle

> because 'tis the Origin of the Root... The Radicle is likewise called  the
seminal Root.
> - Vallemont, *Curiosities of nature and art in husbandry and gardening* (1707)

A LISP in the spirit of [Black](http://pllab.is.ocha.ac.jp/~asai/Black/) and
other colors. Easily define and interact with shared and upgradable state
machines.

## Installation

Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). Then:
```
stack build
```

(You'll need libpq-dev/postgresql to build the server.)

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
stack exec radicle-client -- --config rad/repl.rad
```

## More

Radicle has a [webpage](radicle.xyz), and a [guide](docs.radicle.xyz) which
contain a lot more information on `radicle`.

