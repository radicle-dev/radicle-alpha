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
    buildInputs = [ ghc zlib python3 wget stack postgresql glibcLocales moreutils fzf]
      ++ (if doc then [docstuffs postgresql] else [])
      ++ (if extras then [ vimPlugins.stylish-haskell haskellPackages.apply-refact hlint ] else []);
    libraryPkgconfigDepends = [ zlib ];
    shellHook = ''
      export PATH=$PATH:`stack path --local-bin`
      eval $(grep export ${ghc}/bin/ghc)
      export STACKNIXPKGS="--nix-packages 'zlib fzf moreutils'"
      alias check="pushd $PWD && ./scripts/check-fmt.sh && hlint . && popd"
      alias mkdocs="pushd $PWD/docs && make html && popd"
      alias sb="stack build --fast --system-ghc $STACKNIXPKGS"
      alias sbs="stack build --fast --system-ghc $STACKNIXPKGS radicle:exe:radicle-server"
      alias sbrad="stack build --fast --system-ghc $STACKNIXPKGS radicle:exe:radicle"
      alias st="stack test --fast --system-ghc $STACKNIXPKGS"
      alias sts="stack test --fast --system-ghc $STACKNIXPKGS radicle:spec"
      alias str="stack test --fast --system-ghc $STACKNIXPKGS radicle:spec --ta '--pattern \"Radicle source file tests\"'"
      alias server="stack exec radicle-server -- "
      alias rad="stack exec radicle $STACKNIXPKGS -- rad/repl.rad"
    '';
}
