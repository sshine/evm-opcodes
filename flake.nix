{
  description = "Opcode types for Ethereum Virtual Machine (EVM)";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, self', pkgs, ... }: let
        package = import ./default.nix { inherit pkgs; };
        buildTools = with pkgs.haskellPackages; [
          stack
          cabal-install
          haskell-language-server
          hlint
          ghcid
          hpack
          tasty-discover
        ];
      in {
        packages.default = package;
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = _: [ (pkgs.haskell.lib.doBenchmark package) ];
          doBenchmark = true;
          nativeBuildInputs = buildTools;
        };
      };
    };
}
