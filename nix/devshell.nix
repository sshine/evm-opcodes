# A single dev shell covering both languages. Built on `shellFor` so that
# `cabal build`/`cabal test` see the Haskell dependency set from Nix; the Rust
# toolchain and the shared tooling (treefmt, hk, just) are added on top.
{ ... }:
{
  perSystem =
    { config, pkgs, ... }:
    let
      package = import ../default.nix { inherit pkgs; };
      rust-toolchain = pkgs.rust-bin.fromRustupToolchainFile ../rust-toolchain.toml;
    in
    {
      devShells.default = pkgs.haskellPackages.shellFor {
        packages = _: [ (pkgs.haskell.lib.doBenchmark package) ];
        doBenchmark = true;

        nativeBuildInputs =
          (with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ghcid
            hpack
            tasty-discover
            stack
          ])
          ++ [
            rust-toolchain
            config.treefmt.build.wrapper
            config.hk-nix.package
            pkgs.just
            pkgs.deadnix
          ];

        shellHook = config.hk-nix.shellHook;
      };
    };
}
