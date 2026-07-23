//! The [`PositionalOpcode`] type expresses EVM opcodes where jumps and
//! jumpdests are annotated with the byte position of the translated opcode.
//!
//! This representation is useful when generating code that refers to the size
//! of itself or other chunks of code. For example, the `CODECOPY` segment of a
//! contract must refer to the size of the code being copied, and determining
//! the size of a jump is trivial when it is annotated with the destination
//! address.

use crate::{Opcode, PlainOpcode, U256, jump, jumpdest, jumpi, opcode_size};

/// The byte position of an opcode.
pub type Position = u64;

/// A [`PositionalOpcode`] has byte positions annotated at `JUMP`, `JUMPI` and
/// `JUMPDEST`; on `JUMP` and `JUMPI` the positions denote where they jump to,
/// and on `JUMPDEST` they denote the position of the opcode itself.
pub type PositionalOpcode = Opcode<Position>;

/// Translate a slice of [`PositionalOpcode`]s into plain opcodes by converting
/// each jump position into a `PUSH` instruction.
pub fn translate(opcodes: &[PositionalOpcode]) -> Vec<PlainOpcode> {
    opcodes.iter().flat_map(inline).collect()
}

fn inline(opcode: &PositionalOpcode) -> Vec<PlainOpcode> {
    match opcode {
        Opcode::Jump(addr) => vec![Opcode::Push(U256::from(*addr)), jump()],
        Opcode::Jumpi(addr) => vec![Opcode::Push(U256::from(*addr)), jumpi()],
        Opcode::Jumpdest(_) => vec![jumpdest()],
        other => vec![other.concrete()],
    }
}

/// The size of a jump to some absolute position.
pub fn jump_size(pos: Position) -> u64 {
    opcode_size(&Opcode::Push(U256::from(pos))) + opcode_size(&jump())
}
