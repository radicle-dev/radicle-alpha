# Radicle

A peer-to-peer stack for code collaboration.

- **peer-to-peer** : The Radicle stack builds on IPFS, and works without a centralized server. No hassle with setting up your own hosting, and no need to trust companies with your data.
- **terminal-first** : A Radicle project contains a git repository, plus the associated issues and proposals. Access all the issues and proposals associated with your codebase right from your terminal.
- **programmable** : Ever wanted to tweak your code collaboration service? With Radicle each unit of functionality –a machine– is its own litte P2P program, written in the Radicle language. You can change them to suit your needs, or create entirely new ones.


Radicle has a [webpage](http://www.radicle.xyz/) which contains a lot more information on `Radicle`.

## Installation

To build Radicle from source you will need [`stack`][stack].

And make sure the location at which stack installs executables is in your
`PATH`: `export PATH=$HOME/.local/bin:$PATH`.

```
stack build
stack install :rad :radicle
```

Note: `stack` will need about 4GB of memory to compile successfully.

To use Radicle you will also need to install [`ipfs`][ipfs] and
[`git-remote-ipfs`][git-remote-ipfs]. Running Radicle requires you to keep both the
Radicle daemon and Radicle IPFS daemon running.

```
rad daemon-ipfs
rad daemon-radicle
```

[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[ipfs]: https://docs.ipfs.io/guides/guides/install/
[git-remote-ipfs]: https://github.com/oscoin/ipfs/tree/master/git-remote-ipfs#install

### Debian/Ubuntu

We provide `.deb` packages for Debian-based systems.

    wget https://storage.googleapis.com/static.radicle.xyz/releases/radicle_latest_amd64.deb
    sudo apt install ./radicle_latest_amd64.deb

To use Radicle you need to start the Radicle daemon

    systemctl --user start radicle-daemon
    systemctl --user status radicle-daemon

## Issues

We are currently using `Radicle` itself to manage issues (but you
can still submit issues on Github). You can create and see issues with `rad issues list`
in the project repo. To checkout the project, run:

```
rad project checkout 12D3KooWPS3UXcvSZSXfi7P4J9Ut8MMVNvN63HHiCSP8rxj3RmtC
```

If you cloned the project from Github, you can instead, from the repo, run:

```
git config radicle.project-id 12D3KooWPS3UXcvSZSXfi7P4J9Ut8MMVNvN63HHiCSP8rxj3RmtC
```

You can also reach us on the `radicle` IRC channel on `#freenode`, or via the
[mailing list](https://groups.google.com/a/monadic.xyz/forum/#!forum/radicle).


## Development

The script `./scripts/ci-tests.sh` runs all tests that are run on CI. The script
requires [`docker`][docker] and [`docker-compose`][docker-compose] to be
installed for end-to-end tests.

The documentation is build with `make -C docs html`. Reference documentation for
Radicle code must be regenerated with `stack run radicle-doc-ref` and checked
into version control.

### End-to-end Tests

The end-to-end test suite is run with

    RAD_IPFS_API_URL=http://localhost:19301 \
      RAD_BIN="$(stack path --docker --local-install-root)/bin" stack test :e2e

It requires you to first start up an IPFS test network and the Radicle daemon.

    docker-compose -f test/docker-compose.yaml up -d ipfs-test-network
    RAD_IPFS_API_URL=http://localhost:19301 stack exec -- \
      rad-daemon-radicle --machine-config /tmp/radicle-machines.json

And to build the project with stack's docker support:

    stack build --docker

If you use `docker-compose up` for the first time you will also need to
initialize the IPFS test network with

    echo '{"radicle": true}' | \
      docker-compose -f test/docker-compose.yaml exec -T ipfs-test-network ipfs dag put

If you are using `docker-machine`, replace `localhost` in `RAD_IPFS_API_URL`
with the output of `docker-machine ip`.

You can reset the test daemon’s machine configuration by removing the file
`/tmp/radicle-machines.json`.

### Packaging

Packages can be built with the `./packaging/build-package.sh` script. Run it
with `-h` for more information. The script requires [`fpm`][fpm].

On CI a Debian package is built for every commit and uploaded to
`http://static.radicle.xyz/releases`. The package uses the commit hash as the
version.

[fpm]: https://github.com/jordansissel/fpm

### Troubleshooting

Your local machine might build binaries that are incompatible with the
`debian:stretch` container image. In that case building the docker images fails.
You can build compatible binaries using stack’s [docker
integration][stack-docker-integration]. This is enabled by passing the
`STACK_DOCKER=1` environment to `./scripts/ci-tests.sh`.


[stack-docker-integration]: https://docs.haskellstack.org/en/stable/docker_integration/
[docker]: https://www.docker.com/get-started
[docker-compose]: https://docs.docker.com/compose/install


## Code of conduct

Please read our [code of conduct](code-of-conduct.md) when thinking of contributing.
