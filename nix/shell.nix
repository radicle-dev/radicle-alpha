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
    buildInputs = [ ghc zlib python3 wget stack postgresql glibcLocales ]
      ++ (if doc then [docstuffs postgresql] else [])
      ++ (if extras then [ vimPlugins.stylish-haskell haskellPackages.apply-refact hlint ] else []);
    libraryPkgconfigDepends = [ zlib ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      alias check="pushd $PWD && ./scripts/check-fmt.sh && hlint . && popd"
      alias mkdocs="pushd $PWD/docs && make html && popd"
      alias sb="stack build --fast --system-ghc --nix-packages zlib"
      alias sbs="stack build --fast --system-ghc --nix-packages zlib radicle:exe:radicle-server"
      alias sbc="stack build --fast --system-ghc --nix-packages zlib radicle:exe:radicle-client"
      alias st="stack test --fast --system-ghc --nix-packages zlib"
      alias sts="stack test --fast --system-ghc --nix-packages zlib radicle:spec"
      alias str="stack test --fast --system-ghc --nix-packages zlib radicle:spec --ta '--pattern \"Radicle source file tests\"'"
      alias server="stack exec radicle-server -- "
      alias client="stack exec radicle -- rad/repl.rad"
    '';
}
