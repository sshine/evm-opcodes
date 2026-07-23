config := "--enable-tests --enable-benchmarks"

# List available recipes.
default:
    @just --list

# Format the whole tree (rustfmt, nixfmt, mdformat, fourmolu).
fmt:
    treefmt

# Check formatting without modifying files (CI).
fmt-check:
    treefmt --fail-on-change --no-cache

# hpack sync + Haskell/Rust version equality (and tag match under GITHUB_REF).
sync:
    bash scripts/check-sync.sh

# Lint both languages.
lint:
    hlint haskell/evm-opcodes/src/
    cargo clippy --all-targets --all-features -- -D warnings

# Build both languages.
build:
    cabal build all {{ config }}
    cargo build --workspace

# Test both languages.
test:
    cabal test all {{ config }} --test-show-details=always
    cargo test --workspace

# Run the Haskell benchmarks.
bench:
    cabal bench all {{ config }}

# Build documentation for both languages.
doc:
    cabal haddock all {{ config }}
    cargo doc --workspace --no-deps

# Everything CI runs.
ci: sync fmt-check lint test build doc
