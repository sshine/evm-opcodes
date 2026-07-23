#!/usr/bin/env bash
# Assert that the generated cabal file is in sync with package.yaml and that the
# Haskell and Rust packages share one version. On a `v*` tag, also assert the
# tag matches that shared version. Run as a pre-push hook and in CI.
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

hs_dir="haskell/evm-opcodes"

# 1. hpack: the checked-in .cabal must match package.yaml.
( cd "$hs_dir" && hpack )
if ! git diff --exit-code "$hs_dir/evm-opcodes.cabal"; then
  echo "Error: $hs_dir/evm-opcodes.cabal is out of sync with package.yaml"
  echo "Run 'hpack' in $hs_dir and commit the changes."
  exit 1
fi
echo "$hs_dir/evm-opcodes.cabal is in sync with package.yaml"

# 2. The Haskell and Rust versions must be equal (lockstep releases).
hs_version=$(grep '^version:' "$hs_dir/package.yaml" | awk '{print $2}')
rust_version=$(grep -m1 '^version' Cargo.toml | sed -E 's/.*"([^"]+)".*/\1/')

if [[ "$hs_version" != "$rust_version" ]]; then
  echo "Error: Haskell version ($hs_version) does not match Rust version ($rust_version)"
  echo "Both packages are released in lockstep and must share one version."
  exit 1
fi
echo "Haskell and Rust versions match ($hs_version)"

# 3. On a version tag, the tag must match the shared version.
if [[ "${GITHUB_REF:-}" == refs/tags/v* ]]; then
  tag_version="${GITHUB_REF#refs/tags/v}"
  if [[ "$tag_version" != "$hs_version" ]]; then
    echo "Error: tag version ($tag_version) does not match package version ($hs_version)"
    exit 1
  fi
  echo "Tag version matches package version ($hs_version)"
fi
