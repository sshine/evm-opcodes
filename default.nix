{ pkgs, ... }:

pkgs.haskellPackages.callCabal2nix "evm-opcodes" ./. {}
