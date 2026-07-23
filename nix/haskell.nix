# The Haskell package, built with callCabal2nix (see ../default.nix). It is also
# the flake's default package, preserving the pre-workspace `nix build` behaviour.
{ ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      package = import ../default.nix { inherit pkgs; };
    in
    {
      packages.evm-opcodes-hs = package;
      packages.default = package;
    };
}
