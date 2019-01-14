let
  config =  {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          radicle =
            haskellPackagesNew.callPackage ./default.nix { };

          doctest =
            pkgs.haskell.lib.doJailbreak haskellPackagesOld.doctest;

        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

in
  pkgs.haskellPackages.radicle
