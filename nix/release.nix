let
  config =  {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          radicle =
            haskellPackagesNew.callPackage ./default.nix { };

          tdigest =
            pkgs.haskell.lib.doJailbreak haskellPackagesOld.tdigest;

          ipld-cid =
            haskellPackagesNew.callPackage ./ipld-cid.nix { };

          binary-varint =
            haskellPackagesNew.callPackage ./binary-varint.nix { };

          multibase =
            haskellPackagesNew.callPackage ./multibase.nix { };

          multihash-cryptonite =
            haskellPackagesNew.callPackage ./multihash-cryptonite.nix { };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

in pkgs.haskellPackages.radicle
