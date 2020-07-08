# hs-evm-opcodes

This Haskell library provides opcode types for the Ethereum Virtual Machine (EVM).

The library has two purposes:

 - To provide an interface between EVM-related libraries to lower the cost of
   interoperability. This library has a very low ambition and dependency footprint.
 - To provide easy translation between labelled/positional jumps. Labelled jumps
   can be most useful when generating EVM code, but actual EVM jump instructions
   pop the address from the stack.

The library has these types:

 - [`LabelledOpcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/LabelledOpcode.hs),
 - [`PositionedOpcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/PositionedOpcode.hs),
 - [`Opcode`](https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/Opcode.hs),

And a [fixpoint algorithm][fixpoint] that translates labels into positions:

[fixpoint]: https://github.com/sshine/hs-evm-opcodes/blob/master/src/Network/Ethereum/Evm/LabelledOpcode.hs#L49

```haskell
LabelledOpcode.translate :: [LabelledOpcode] -> Either TranslateError [PositionedOpcode]
PositionedOpcode.translate :: [PositionedOpcode] -> [Opcode]
```
