{ pkgs, ... }: let
  haskellDependencies = with pkgs.haskellPackages; [
    stack
    cabal-install
    haskell-language-server
    hlint
    ghcid
    hpack
  ];

  # Build a Haskell package with those dependencies
  addBuildTools = pkgs.haskell.lib.addBuildTools;
  package = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv: addBuildTools drv haskellDependencies;
  };
in
  package
