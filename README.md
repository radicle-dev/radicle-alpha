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

You will need [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed.

Then run:

```
stack build
stack install :rad
```

Note: `stack` will need about 4GB of memory to compile successfully.

## Usage

```
radicle ./rad/repl.rad
rad> (print! "hello world")
rad> (def-rec fac (fn [n] (if (eq? n 0) 1 (* n (fac (- n 1))))))
rad> (fac 6)
```

## Issues

We are currently using `radicle` itself to manage issues, and have therefore
disabled issues on Github. You can create and see issues with the
`bin/rad-issues` script. You can also reach us on the `radicle` IRC channel on
`#freenode`.

## Development

The script `./scripts/ci-tests.sh` runs all tests that are run on CI. The script
requires [`docker`][docker] and [`docker-compose`][docker-compose] to be
installed for end-to-end tests.

The documentation is build with `make -C docs html`. Reference documentation for
Radicle code must be regenerated with `stack run radicle-doc-ref` and checked
into version control.

### End-to-end Tests

The end-to-end test suite is run with `stack test :e2e`. It requires you to
start up an IPFS test network and the Raicle daemon.

    docker-compose -f test/docker-compose.yaml up -d ipfs-test-network
    RAD_IPFS_API_URL=http://localhost:19301 stack exec -- \
      rad-daemon-radicle --machine-config /tmp/radicle-machines.json

If you use `docker-compose up` for the first time you will also need to
initialize the IPFS test network with

    echo '{"radicle": true}' | \
      docker-compose -f test/docker-compose.yaml exec -T ipfs-test-network ipfs dag put

You can reset the test daemon’s machine configuration by removing the file
`/tmp/radicle-machines.json`.

### Troubleshooting

Your local machine might build binaries that are incompatible with the
`debian:stretch` container image. In that case building the docker images fails.
You can build compatible binaries using stack’s [docker
integration][stack-docker-integration]. This is enabled by passing the
`STACK_DOCKER=1` environment to `./scripts/ci-tests.sh`.


[stack-docker-integration]: https://docs.haskellstack.org/en/stable/docker_integration/
[docker]: https://www.docker.com/get-started
[docker-compose]: https://docs.docker.com/compose/install
