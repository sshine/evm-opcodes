# evm-opcodes

This Haskell library provides opcode types for the Ethereum Virtual Machine. Mainly it provides three types,
[`PositionedOpcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/PositionedOpcode.hs),
[`LabelledOpcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/LabelledOpcode.hs)
and the plain [`Opcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/Opcode.hs),
and a fixpoint algorithm to translate between them. This gives the Ethereum compiler writer a neat interface to labelled
jumps and a way to translate them into absolute addressing.

The library is intended to work as a lightweight interoperability layer between Ethereum-related libraries.
