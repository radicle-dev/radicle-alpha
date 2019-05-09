# Revision history for radicle

## 0.0.4  -- 2019-05-09

### Added

* Machines can now define a `(get-html)`; if that returns a IPFS CID, then
  querying `v0/machines/<machine-id>/frontend/<path>` will return the result of
  querying `CID/<path>` on IPFS. This allows easily making frontends for
  machines ([PR](https://github.com/radicle-dev/radicle/pull/630)).

### Fixed

* Pubsub was not restarted if connection was lost. Now we try restarting, with
  backoff ([PR](https://github.com/radicle-dev/radicle/pull/634)).

## 0.0.3  -- 2019-04-12

### Fixed

* Pubsub (and therefore writing by non-owner) was broken. Now it's fixed ([PR](https://github.com/radicle-dev/radicle/pull/620)).

## 0.0.2  -- 2019-04-10

### Added

* Added `rad repl` command ([PR](https://github.com/radicle-dev/radicle/pull/614))
* Support multiple expressions in :setup ([PR](https://github.com/radicle-dev/radicle/pull/609))
* Support lambda definition with an atom representing an arguments vector ([PR](https://github.com/radicle-dev/radicle/pull/606))

### Changed

* Increase ACK timeouts ([PR](https://github.com/radicle-dev/radicle/pull/604))
* Print connection error when client can't connect to radicle daemon. ([PR](https://github.com/radicle-dev/radicle/pull/603))
* Rad daemon logging changes ([PR](https://github.com/radicle-dev/radicle/pull/602))

### Fixed

* Reading machines for the first time was broken ([PR](https://github.com/radicle-dev/radicle/pull/617))
* Proper line counts with shebangs ([PR](https://github.com/radicle-dev/radicle/pull/605))
* Usage text fixes ([PR](https://github.com/radicle-dev/radicle/pull/598))
* Use new version of remote helper for 0.4.19 fix ([PR](https://github.com/radicle-dev/radicle/pull/613))

## 0.0.1  -- 2019-03-27

* Initial fixed released. 0.0.0 was moving, so a changelog is not appropriate.

## 0.0.0  -- ???

* First version.
