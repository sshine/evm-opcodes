//! A generic method of traversing [`Opcode`]s.

use crate::Opcode;

/// An [`OpcodeMapper`] maps any `Opcode<A>` to an `Opcode<B>`, possibly failing
/// with an error `E`.
///
/// For each of the three annotated opcodes — `JUMP`, `JUMPI` and `JUMPDEST` — a
/// separate mapping function is provided. For any other opcode, [`on_other`]
/// may return a replacement, or `None` to fall back to the same opcode rebuilt
/// as an `Opcode<B>`.
///
/// This is the Rust counterpart of the Haskell `OpcodeMapper` record. The
/// Haskell version is generic over an arbitrary monad `m`; here it is
/// specialised to [`Result`], which covers the only in-tree use — jump-label
/// resolution in [`crate::labelled`].
///
/// [`on_other`]: OpcodeMapper::on_other
pub trait OpcodeMapper<A, B> {
    /// The error type produced by a failed mapping.
    type Error;

    /// Map the annotation of a `JUMP`.
    fn on_jump(&self, a: A) -> Result<Opcode<B>, Self::Error>;
    /// Map the annotation of a `JUMPI`.
    fn on_jumpi(&self, a: A) -> Result<Opcode<B>, Self::Error>;
    /// Map the annotation of a `JUMPDEST`.
    fn on_jumpdest(&self, a: A) -> Result<Opcode<B>, Self::Error>;
    /// Map any other opcode. Returning `None` falls back to the same opcode.
    fn on_other(&self, opcode: Opcode<A>) -> Result<Option<Opcode<B>>, Self::Error>;
}

/// Given an [`OpcodeMapper`] and an `Opcode<A>`, produce an `Opcode<B>`.
pub fn map_opcode<A, B, M>(mapper: &M, opcode: Opcode<A>) -> Result<Opcode<B>, M::Error>
where
    A: Clone,
    M: OpcodeMapper<A, B>,
{
    match opcode {
        Opcode::Jump(a) => mapper.on_jump(a),
        Opcode::Jumpi(a) => mapper.on_jumpi(a),
        Opcode::Jumpdest(a) => mapper.on_jumpdest(a),
        other => {
            // `other` is guaranteed non-jump here, so the closure is never run.
            let default = other.clone().map_annot(|_| unreachable!());
            Ok(mapper.on_other(other)?.unwrap_or(default))
        }
    }
}
