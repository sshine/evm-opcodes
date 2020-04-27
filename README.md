# hs-evm-opcodes

This library provides opcode types for the Ethereum Virtual Machine.

Mainly it provides two types, [`PositionedOpcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/PositionedOpcode.hs)
and [`LabelledOpcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/LabelledOpcode.hs), and a fixpoint algorithm to
translate between them. This gives the Ethereum compiler writer a neat interface to labelled jumps and a way to translate them into absolute addressing.
