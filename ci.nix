with (import <nixpkgs> {});
{
  radicle = haskellPackages.callPackage ./nix/default.nix {};
}
