#!/usr/bin/env bash
set -euo pipefail

hpack

if ! git diff --exit-code evm-opcodes.cabal; then
  echo "Error: evm-opcodes.cabal is out of sync with package.yaml"
  echo "Run 'hpack' and commit the changes."
  exit 1
fi

echo "evm-opcodes.cabal is in sync with package.yaml"
