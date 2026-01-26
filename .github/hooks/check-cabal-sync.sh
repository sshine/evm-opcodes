#!/usr/bin/env bash
set -euo pipefail

hpack

if ! git diff --exit-code evm-opcodes.cabal; then
  echo "Error: evm-opcodes.cabal is out of sync with package.yaml"
  echo "Run 'hpack' and commit the changes."
  exit 1
fi

echo "evm-opcodes.cabal is in sync with package.yaml"

if [[ "${GITHUB_REF:-}" == refs/tags/v* ]]; then
  tag_version="${GITHUB_REF#refs/tags/v}"
  pkg_version=$(grep '^version:' package.yaml | awk '{print $2}')

  if [[ "$tag_version" != "$pkg_version" ]]; then
    echo "Error: tag version ($tag_version) does not match package.yaml version ($pkg_version)"
    exit 1
  fi

  echo "Tag version matches package.yaml ($pkg_version)"
fi
