{ pkgs ? import (fetchGit {
    url = https://github.com/nixos/nixpkgs.git;
    ref = "release-19.03";
    rev = "f52505fac8c82716872a616c501ad9eff188f97f";
  }) {}
, extras ? []
}:

with pkgs;

haskell.lib.buildStackProject {
  inherit ghc;
  name = "radicle";
  buildInputs = [ git zlib ipfs docker docker_compose ] ++ extras ;
}
