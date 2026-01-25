let
  # Use nixpkgs available on the system
  pkgs = import <nixpkgs> {};
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

  # Extend the build environment with pre-commit installed and activated
  withPreCommitHook = old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.pre-commit ];
    shellHook = (old.shellHook or "") + ''
      if [ -d .git ]; then
        pre-commit install --hook-type pre-push
      fi
    '';
  };
in
  package.overrideAttrs withPreCommitHook
