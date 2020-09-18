# evm-opcodes

This Haskell library provides opcode types for the Ethereum Virtual Machine (EVM).

The library has two purposes:

 - To provide an interface between EVM-related libraries to lower the cost of
   interoperability. This library has a very low ambition and dependency footprint.
 - To provide easy translation between labelled/positional jumps. Labelled jumps
   can be most useful when generating EVM code, but actual EVM jump instructions
   pop the address from the stack.

The library has one abstract type, `Opcode' a` where `a` is the annotation for
the jump-related instructions `JUMP`, `JUMPI` and `JUMPDEST`, and it has three
concrete variants:

 - `Opcode` being `Opcode' ()`
 - `PositionalOpcode` being `Opcode' Word256`
 - `LabelledOpcode` being `Opcode' Label` with `Label` being an alias for `Text`.

The library has a fixpoint algorithm that translates labels into positions.

## Example

Imagine translating the following C program to EVM opcodes:

```
int x = 1;
while (x != 0) { x *= 2 };
```

Since EVM is stack-based, let's put `x` on the stack.

```haskell
λ> import EVM.Opcode
λ> import EVM.Opcode.Labelled as L
λ> import EVM.Opcode.Positional as P

λ> let opcodes = [PUSH 1,JUMPDEST "loop",DUP1,ISZERO,JUMPI "end",PUSH 2,MUL,JUMP "loop",JUMPDEST "end"]

λ> L.translate opcodes
Right [PUSH 1,JUMPDEST 2,DUP1,ISZERO,JUMPI 14,PUSH 2,MUL,JUMP 2,JUMPDEST 14]

λ> P.translate <$> L.translate opcodes
Right [PUSH 1,JUMPDEST,DUP1,ISZERO,PUSH 14,JUMPI,PUSH 2,MUL,PUSH 2,JUMP,JUMPDEST]

λ> fmap opcodeText . P.translate <$> L.translate opcodes
Right ["push1 1","jumpdest","dup1","iszero","push1 14","jumpi","push1 2","mul","push1 2","jump","jumpdest"]
```
