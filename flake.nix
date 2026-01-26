{
  description = "Opcode types for Ethereum Virtual Machine (EVM)";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, self', pkgs, ... }: {
        packages.default = import ./default.nix { inherit pkgs; };
      };
    };
}
