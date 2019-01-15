{ mkDerivation, aeson, ansi-terminal, base, bytestring, containers
, criterion, cryptonite, data-default, directory, doctest
, filemanip, filepath, generics-eot, github, Glob, haskeline, hpack
, http-client, interpolate, ipld-cid, megaparsec, microlens, mtl
, optparse-applicative, pandoc, pointed, postgresql-simple
, prettyprinter, prettyprinter-ansi-terminal, process, protolude
, QuickCheck, quickcheck-instances, safe-exceptions, scientific
, serialise, servant, servant-client, servant-server, stdenv
, string-qq, tasty, tasty-discover, tasty-hunit, tasty-quickcheck
, template-haskell, text, time, unordered-containers, uuid
, wai-cors, wai-extra, warp, wreq, yaml
}:
mkDerivation {
  pname = "radicle";
  version = "0.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring containers cryptonite directory
    filepath generics-eot haskeline http-client ipld-cid megaparsec
    microlens mtl pointed prettyprinter prettyprinter-ansi-terminal
    process protolude safe-exceptions scientific serialise servant
    servant-client template-haskell text time unordered-containers uuid
    wreq
  ];
  executableHaskellDepends = [
    aeson base bytestring containers criterion data-default directory
    github optparse-applicative pandoc postgresql-simple protolude
    servant servant-server text time wai-cors wai-extra warp yaml
  ];
  testHaskellDepends = [
    base containers cryptonite directory doctest filemanip Glob hpack
    interpolate megaparsec process protolude QuickCheck
    quickcheck-instances scientific serialise string-qq tasty
    tasty-discover tasty-hunit tasty-quickcheck text time
  ];
  license = stdenv.lib.licenses.mit;
}
