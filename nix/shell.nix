{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc863"
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
    buildInputs = [ ghc zlib glibcLocales python3 wget stack postgresql moreutils fzf docker_compose]
      ++ (if doc then [docstuffs postgresql] else [])
      ++ (if extras then [ vimPlugins.stylish-haskell haskellPackages.apply-refact hlint ] else []);
    LANG = "en_US.UTF-8";
    libraryPkgconfigDepends = [ zlib ];
    shellHook = ''
      export PATH=$PATH:`stack path --local-bin`:$PWD/bin
      export RADPATH=$PWD/rad
      export STACK_IN_NIX_EXTRA_ARGS="--system-ghc --nix-packages zlib"
      export IS_NIX_SHELL="true"
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
      eval $(grep export ${ghc}/bin/ghc)
      alias check="pushd $PWD && ./scripts/check-fmt.sh && hlint . && popd"
      alias mkdocs="pushd $PWD/docs && make html && popd"
      alias sb="stack build --fast"
      alias se="stack exec"
      alias se="stack --no-nix-pure exec"
      alias sbd="stack build --fast radicle:exe:rad-daemon-radicle"
      alias sbrad="stack build --fast radicle:exe:radicle"
      alias st="stack test --fast"
      alias sts="stack test --fast radicle:spec"
      alias str="stack test --fast radicle:spec --ta '--pattern \"Radicle source file tests\"'"
      alias server="stack exec radicle-server -- "
      alias rad="stack exec --silent radicle -- rad/repl.rad"
    '';
}
