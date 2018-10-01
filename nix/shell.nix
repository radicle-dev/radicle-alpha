{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc843"
, doc ? false
}:

with pkgs;

let
  ghc = haskell.packages.${compiler}.ghcWithPackages (_: []);
  docstuffs = python3.withPackages (ps: with ps; [ recommonmark sphinx sphinx_rtd_theme ]);
in

stdenv.mkDerivation {
    name = "radicle-dev";
    buildInputs = [ ghc zlib python3 wget stack ]
      ++ (if doc then [docstuffs postgresql] else []);
    libraryPkgconfigDepends = [ zlib ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      export LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH;
    '';
}
