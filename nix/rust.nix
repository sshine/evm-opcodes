# The Rust crate, built with buildRustPackage against the workspace lockfile.
{ ... }:
{
  perSystem =
    { pkgs, lib, ... }:
    {
      packages.evm-opcodes-rust = pkgs.rustPlatform.buildRustPackage {
        pname = "evm-opcodes";
        version = "0.4.0";
        src = lib.cleanSource ../.;
        cargoLock.lockFile = ../Cargo.lock;
        buildAndTestSubdir = "rust/evm-opcodes";
      };
    };
}
