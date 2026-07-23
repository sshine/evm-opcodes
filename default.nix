{ pkgs, ... }:

pkgs.haskellPackages.callCabal2nix "evm-opcodes" ./haskell/evm-opcodes { }
