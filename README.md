# radicle

> because 'tis the Origin of the Root... The Radicle is likewise called the seminal Root.
> — Vallemont, *Curiosities of nature and art in husbandry and gardening* (1707)

A LISP in the spirit of [Black](http://pllab.is.ocha.ac.jp/~asai/Black/) and
other colors. Easily define and interact with shared and upgradable state
machines.

Radicle has a [webpage](http://radicle.xyz/), and a [Getting Started
Guide](http://docs.radicle.xyz/en/latest/guide/GettingStarted.html) which
contain a lot more information on `radicle`.

## Installation

You need [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) and `libpq-dev`.

```
stack install
```

## Usage

```
radicle ./rad/repl.rad
rad> (print! "hello world")
rad> (def-rec fac (fn [n] (if (eq? n 0) 1 (* n (fac (- n 1))))))
rad> (fac 6)
```

## Radicle Server

`radicle-server` is a service that hosts Radicle state machines and persists
them to PostgreSQL.

The latest master build of this service is distributed as the Docker image
`eu.gcr.io/opensourcecoin/radicle-server:latest`. For every master commit we
also provide an image tagged the current date and a short commit hash. For
example `eu.gcr.io/opensourcecoin/radicle-server:b2018.12.06-a76a52f`.

To build the image locally run `images/radicle-server/build.sh`. You can also
use `images/radicle-server/docker-compose.yaml`.

## Development

You can run tests with `stack test`.

The documentation is build with `make -C docs html`. Reference documentation for
Radicle code must be regenerated with `stack run radicle-doc-ref` and checked
into version control.
