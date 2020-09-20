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
 - `PositionalOpcode` being `Opcode' Word`
 - `LabelledOpcode` being `Opcode' Label` with `Label` being an alias for `Text`.

The library has a fixpoint algorithm that translates labelled jumps into
positional jumps, and it has another function that translates those positional
jumps into plain EVM opcodes where a constant is pushed before a jump is made.

## Naming opcodes

TODO:
 - Describe `DUP...`, `SWAP...`, `LOG...` synonyms.
 - Describe how the library and its docs refer to `PUSH`, `push1`, etc.

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

## Uses the right `push` instruction for absolute jumps

When the byte address of a `jumpdest` exceeds a byte boundary of 255, 65535,
and so on, then performing an absolute jump by pushing a constant that refers
to that address uses more space.  This means that for absolute jumps (e.g.
labelled jumps in a code generator), one must pick the right instructions among
`push1`, `push2`, etc.

This becomes a problem when a `jump` occurs before a `jumpdest`, since then the
size of the `jump` will depend on its own presence in the code. And if there
are many such `jump`s, this amounts to a bit of book-keeping.

Expanding the gap of intermediate instructions between the `jump` and the
`jumpdest`, e.g. as demonstrated below with `STOP` instructions, `L.translate`
correctly computes the size of a labelled jump: Adding one `STOP` from 252 to
253 appears to be only adding a one-byte instruction, but `JUMPDEST "skip"` has
now skipped a boundary:

With 252 `SKIP`s there are 254 instructions that preceeds `JUMPDEST "skip"`: 3
spent by `JUMP "skip"` (because it translates to `push1 255` and `jump`) and
252 `skip`s.

With 253 `SKIP`s there are 257 instructions that preceeds `JUMPDEST "skip"`: 4
spent by `JUMP "skip"` (because it translates to `push2 257` and `jump`) and
253 `skip`s.

```haskell
λ> import EVM.Opcode
λ> import EVM.Opcode.Labelled as L
λ> import EVM.Opcode.Positional as P

λ> fmap opcodeText . P.translate <$> L.translate ([JUMP "skip"] <> replicate 252 STOP <> [JUMPDEST "skip"])
Right ["push1 255","jump","stop","stop","stop",...,"jumpdest"]

λ> fmap opcodeText . P.translate <$> L.translate ([JUMP "skip"] <> replicate 253 STOP <> [JUMPDEST "skip"])
Right ["push2 257","jump","stop","stop","stop",...,"jumpdest"]

λ> fmap opcodeText . P.translate <$> L.translate ([JUMP "skip"] <> replicate 65532 STOP <> [JUMPDEST "skip"])
Right ["push3 65537","jump","stop","stop",...,"jumpdest"]
```
