let
  pkgs = import <nixpkgs> {};
in
  pkgs.haskellPackages.developPackage {
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
  }
