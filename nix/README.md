You can use the `shell.nix` from this directory to build radicle (and
optionally the documentation):

Just radicle:

``` sh
$ nix-shell nix/shell.nix
```

Everything needed for the documentation too:

``` sh
$ nix-shell nix/shell.nix --arg doc true
```

The `shell.nix` file also supports specifying a particular ghc version, e.g:

``` sh
$ nix-shell nix/shell.nix --argstr compiler ghcHEAD
```

(Since stack 1.n7 is not currently on the stable channel, you may need to add
the flag `--arg pkgs "import <nixos-unstable> {}`)

And then, inside the shell:

``` sh
stack build --system-ghc
```
