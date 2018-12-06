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
      export STACK_ARGS="--system-ghc --nix-packages 'zlib fzf moreutils'"
      export IS_NIX_SHELL="true"
      eval $(grep export ${ghc}/bin/ghc)
      alias check="pushd $PWD && ./scripts/check-fmt.sh && hlint . && popd"
      alias mkdocs="pushd $PWD/docs && make html && popd"
      alias sb="stack build --fast $STACK_ARGS"
      alias sbs="stack build --fast $STACK_ARGS radicle:exe:radicle-server"
      alias sbrad="stack build --fast $STACK_ARGS radicle:exe:radicle"
      alias st="stack test --fast $STACK_ARGS"
      alias sts="stack test --fast $STACK_ARGS radicle:spec"
      alias str="stack test --fast $STACK_ARGS radicle:spec --ta '--pattern \"Radicle source file tests\"'"
      alias server="stack exec $STACK_ARGS radicle-server -- "
      alias rad="stack exec --silent $STACK_ARGS radicle -- rad/repl.rad"
    '';
}
