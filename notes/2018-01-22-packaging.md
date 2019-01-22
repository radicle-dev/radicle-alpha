Packaging
=========

This documents explains how we want to package and distribute Radicle.

We will create packages for Brew, Debian, Pacman, and (possibly Nix). This
packages will be made available through Github Releases.

Haskell binaries will be statically linked with `-O2` optimization.

Files to package
----------------

The Radicle package contains the `rad` entry command, the `rad-xxx` subcommands
and Radicle source files.

The `rad` command will be installed into a `PATH` directory. The subcommands and
Radicle source files are installed into distribution specific directories. For
Debian and Pacman this is `/usr/lib/radicle/modules` for the Radicle source
files and `/usr/lib/radicle/bin` for the subcommands.

Locating packaged files
-----------------------

The binaries distributed with the package need to locate files on the system the
package is installed on.

* The `rad` entry command locates all installed subcommands to be able to list
  them and dispatch to them.
* Radicle apps (implemented as subcommands) need to locate Radicle source files.
* If Radicle app frontends that are written in radicle the interpreter also needs
  to locate Radicle source files to be able to import them.

In addition to installed packages the location mechanism needs to work for
development builds (`stack build`), local tests, and CI tests.

To implement file location of subcommands we are going to use Cabal’s
[`Paths_radicle`][cabal-paths] module. `getBinDir` will return the folder
containing all subcommands.

File location for Radicle source files will be implemented by reading the
`RADPATH` environment variable

For local development and testing this mechanisms work out of the box. For
packages the `rad` command will be a script with the following content.

```
#!/bin/sh
radicle_bin=/usr/lib/radicle/bin
RADPATH=/usr/lib/radicle/modules
exec $radicle_bin/rad "@$"
```

Setting `radicle_bin` will determine the return value of `getBinDir`. Setting
`RADPATH` will enable other commands to pick up Radicle source files.

### Configuring the prefix at compile time

If the project is built with Cabal we do not need to set the `radicle_bin`
environment variable to change the output of `getBinDir`. We can use the
`--bindir` option for `cabal configure`.

Stack [does not support the `--bindir` option][stack-prefix-issue] so we have to
work around by setting the corresponding environment variable.

For the first iteration we will use a stack based build because of the
work involved in building the project with cabal. (Defining package constraints
identical to Stackage LTS, publishing Git dependencies, additional CI build
scripts). We will use a Cabal based build in the future.

[cabal-paths]: https://cabal.readthedocs.io/en/latest/developing-packages.html#accessing-data-files-from-package-code
[stack-prefix-issue]: https://github.com/commercialhaskell/stack/issues/848

Dependencies
------------

Radicle depends on the IPFS daemon. Packages must either register the IPFS
daemon as a dependency or distribute the `ipfs` binary with them.

Services
--------

For Radicle to work two services need to be running: the Radicle daemon and the
IPFS daemon. For Debian in Pacman we will provide a Systemd Unit file for the
Radicle daemon as well as installation instructions that include extra steps to
enable the Radicle daemon service.

The Radicle daemon Systemd Unit has a dependency on the IPFS Systemd Unit. If we
distribute IPFS with the package we also need to add a Systemd Unit file for
IPFS.

For Brew on MacOS we will need to provide `plist` files for the services so they
can be used with launchd.

Continuous Delivery
-------------------

Packages will be released using a release pipeline in CI. The release pipeline
will build the packages, create a Github release with the appropriate name and
description, and attach the packages to the Github release. The release pipeline
will be triggered by Git tags.

To build Brew packages we will need to run the build pipeline on MacOS. Google
Cloud Build does not support MacOS but Travis and Circle do.

Release pipeline
----------------

This is how the release pipeline would look for a Debian package. For a Pacman
package only minor modifications would need to be made.

1. `stack build -- --flag radicle:release`. The flag disables some targets,
   enforces static linking and `-O3`.
2. Move all binaries built with stack to `$DEST/usr/lib/radicle/bin`
3. Move all Radicle sources files to `$DEST/usr/lib/radicle/modules`
4. Create `$DEST/usr/bin/rad` wrapper script as mentioned above.
5. Create a `deb` package using [`fpm`][fpm]. `fpm` takes the Systemd file as a CLI option.
6. Upload the `deb` file to Github releases

[fpm]: https://github.com/jordansissel/fpm

Signing release artifacts
-------------------------

We should sign all the release artifacts that we provide. How to accomplish that
is still an open question.

* If we don’t build on our own machines we effectively trust the CI service
* We can add signatures to Github release later. We could download release
  artifacts and sign them locally. CI could provide checksums for additional
  security.
