# Getting Started

The easiest way to play with `radicle` is to use the online REPL, available at
the [`radicle` website](http://radicle.xyz).

The online REPL is quite limited, however (and slow). So you will probably also
want the `radicle` executable.

## Building from source

In order to build from source, you'll need
[stack](https://docs.haskellstack.org/en/stable/README/):

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

You can then download and build the latest source code for `radicle` from
http://git.oscoin.io/radicle:

```bash
git clone http://git.oscoin.io/radicle
cd radicle
stack build
stack install
```

You can test that `radicle` was properly installed by running:

```bash
radicle --help
```
