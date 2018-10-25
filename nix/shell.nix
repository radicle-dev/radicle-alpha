{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc843"
, doc ? true
, extras ? true
}:

with pkgs;

let
  ghc = haskell.packages.${compiler}.ghcWithPackages (_: []);
  docstuffs = python3.withPackages (ps: with ps; [ recommonmark sphinx sphinx_rtd_theme ]);
in

stdenv.mkDerivation {
    name = "radicle-dev";
    buildInputs = [ ghc zlib python3 wget stack ]
      ++ (if doc then [docstuffs postgresql] else [])
      ++ (if extras then [ vimPlugins.stylish-haskell haskellPackages.apply-refact hlint ] else []);
    libraryPkgconfigDepends = [ zlib ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      alias check="pushd $PWD && ./scripts/check-fmt.sh && hlint . && popd"
      alias mkdocs="pushd $PWD/docs && make html && popd"
      alias sb="stack build --system-ghc --nix-packages zlib"
      alias st="stack test --fast --system-ghc --nix-packages zlib"
    '';
}
