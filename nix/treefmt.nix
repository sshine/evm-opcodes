# Formatting for the whole tree: rustfmt, nixfmt, mdformat, fourmolu.
#
# The hand-aligned opcode tables in a few Haskell files are excluded from
# fourmolu so their alignment survives; opt a file back in by removing it from
# `excludes`, or use inline `{- FOURMOLU_DISABLE -}` / `{- FOURMOLU_ENABLE -}`
# regions. To drop Haskell autoformatting entirely, remove `programs.fourmolu`.
{ inputs, ... }:
{
  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem =
    { pkgs, ... }:
    let
      rust-toolchain = pkgs.rust-bin.fromRustupToolchainFile ../rust-toolchain.toml;
    in
    {
      treefmt = {
        projectRootFile = "flake.nix";

        programs.rustfmt = {
          enable = true;
          package = rust-toolchain;
        };
        programs.nixfmt.enable = true;
        programs.mdformat.enable = true;
        programs.fourmolu.enable = true;

        settings.formatter.fourmolu.excludes = [
          "haskell/evm-opcodes/src/EVM/Opcode.hs"
          "haskell/evm-opcodes/src/EVM/Opcode/Internal.hs"
          "haskell/evm-opcodes/src/EVM/Opcode/Traversal.hs"
        ];
      };
    };
}
