let
  pkgs = import <nixpkgs> {};
  package = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
        stack
        cabal-install
        haskell-language-server
        hlint
        ghcid
        hpack
      ]);
  };
in
  package.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.pre-commit ];
    shellHook = (old.shellHook or "") + ''
      if [ -d .git ]; then
        pre-commit install --hook-type pre-push
      fi
    '';
  })
