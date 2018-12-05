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

The lastest master build of this service is distributed as the Docker image
`eu.gcr.io/opensourcecoin/radicle-server`. To build the image locally run
`images/radicle-server/build.sh`. You can also use
`images/radicle-server/docker-compose.yaml`.

## Development

You can run tests with `stack test`.

### Documentation

To build the documentation you will need to install the Python dependencies
```
pip install -r docs/requirements.txt
```

You can then build the documentation with
```
stack run radicle-ref-doc
make -C docs html
```

The `radicle-ref-doc` command generates reference documentation from Radicle
source files.
