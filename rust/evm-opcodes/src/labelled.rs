//! The [`LabelledOpcode`] type expresses EVM opcodes with labelled jumps.
//!
//! Plain EVM opcodes are not ergonomic because one has to know the exact byte
//! offset of the target `JUMPDEST`. With labelled jumps the byte offset is
//! resolved automatically, accounting for the fact that the offset depends on
//! all `PUSH` instructions prior to the label, including the `PUSH` to the
//! label itself.

use core::fmt;
use std::collections::{BTreeSet, HashMap};

use crate::positional::{Position, PositionalOpcode, jump_size};
use crate::traversal::{OpcodeMapper, map_opcode};
use crate::{Opcode, jumpdest, opcode_size};

/// For now, all labels are [`String`]s.
pub type Label = String;

/// A [`LabelledOpcode`] uses a [`Label`] to represent jumps — in particular
/// `JUMP "name"`, `JUMPI "name"` and `JUMPDEST "name"`. All other opcodes
/// remain the same.
pub type LabelledOpcode = Opcode<Label>;

/// Translating [`LabelledOpcode`]s may fail if a jump is made to a
/// non-occurring `JUMPDEST` or a `JUMPDEST` occurs more than once.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TranslateError {
    /// Labels that are jumped to but never defined by a `JUMPDEST`.
    pub missing_jumpdests: Vec<Label>,
    /// Labels defined by more than one `JUMPDEST`.
    pub duplicate_jumpdests: Vec<Label>,
}

impl fmt::Display for TranslateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "translate error: missing jumpdests {:?}, duplicate jumpdests {:?}",
            self.missing_jumpdests, self.duplicate_jumpdests
        )
    }
}

impl std::error::Error for TranslateError {}

/// Replace all labels with absolute positions.
///
/// Positions are calculated by fixed-point iteration to account for the
/// variable size of jumps: labelled jumps have no size defined, and the size of
/// a positional jump depends on the address being jumped to. For example,
/// jumping to a `JUMPDEST` at the 256th position requires a `PUSH2`, which uses
/// an additional byte and pushes the `JUMPDEST` one byte ahead.
pub fn translate(opcodes: &[LabelledOpcode]) -> Result<Vec<PositionalOpcode>, TranslateError> {
    let labels = label_positions(opcodes)?;
    let mapper = LabelReplacer { labels: &labels };
    opcodes
        .iter()
        .map(|op| map_opcode(&mapper, op.clone()))
        .collect()
}

struct LabelReplacer<'a> {
    labels: &'a HashMap<Label, Position>,
}

impl LabelReplacer<'_> {
    fn lookup(&self, label: Label) -> Result<Position, TranslateError> {
        match self.labels.get(&label) {
            Some(&pos) => Ok(pos),
            None => Err(TranslateError {
                missing_jumpdests: vec![label],
                duplicate_jumpdests: vec![],
            }),
        }
    }
}

impl OpcodeMapper<Label, Position> for LabelReplacer<'_> {
    type Error = TranslateError;

    fn on_jump(&self, a: Label) -> Result<Opcode<Position>, TranslateError> {
        Ok(Opcode::Jump(self.lookup(a)?))
    }

    fn on_jumpi(&self, a: Label) -> Result<Opcode<Position>, TranslateError> {
        Ok(Opcode::Jumpi(self.lookup(a)?))
    }

    fn on_jumpdest(&self, a: Label) -> Result<Opcode<Position>, TranslateError> {
        Ok(Opcode::Jumpdest(self.lookup(a)?))
    }

    fn on_other(&self, _opcode: Opcode<Label>) -> Result<Option<Opcode<Position>>, TranslateError> {
        Ok(None)
    }
}

/// Compute a map describing where each `JUMPDEST` is located, taking into
/// account the sizes of all prior opcodes.
pub fn label_positions(
    opcodes: &[LabelledOpcode],
) -> Result<HashMap<Label, Position>, TranslateError> {
    let jumps: Vec<Label> = opcodes
        .iter()
        .filter_map(|op| op.jump_annot().cloned())
        .collect();
    let dests: Vec<Label> = opcodes
        .iter()
        .filter_map(|op| op.jumpdest_annot().cloned())
        .collect();

    let wild_jumps = missing(&jumps, &dests);
    let duplicate_dests = duplicate(&dests);

    if wild_jumps.is_empty() && duplicate_dests.is_empty() {
        Ok(fixpoint(opcodes))
    } else {
        Err(TranslateError {
            missing_jumpdests: wild_jumps,
            duplicate_jumpdests: duplicate_dests,
        })
    }
}

/// The labels in `xs` that do not occur in `ys`, sorted and deduplicated.
fn missing(xs: &[Label], ys: &[Label]) -> Vec<Label> {
    let present: BTreeSet<&Label> = ys.iter().collect();
    xs.iter()
        .filter(|x| !present.contains(x))
        .cloned()
        .collect::<BTreeSet<Label>>()
        .into_iter()
        .collect()
}

/// The labels that occur more than once in `xs`, sorted.
fn duplicate(xs: &[Label]) -> Vec<Label> {
    let mut sorted = xs.to_vec();
    sorted.sort();
    let mut out = Vec::new();
    let mut i = 0;
    while i < sorted.len() {
        let mut j = i + 1;
        while j < sorted.len() && sorted[j] == sorted[i] {
            j += 1;
        }
        if j - i > 1 {
            out.push(sorted[i].clone());
        }
        i = j;
    }
    out
}

/// Iterate [`step`] until the label map stops changing.
///
/// This may not terminate for all inputs.
fn fixpoint(opcodes: &[LabelledOpcode]) -> HashMap<Label, Position> {
    let mut label_map = HashMap::new();
    loop {
        let (done, next) = step(&label_map, opcodes);
        label_map = next;
        if done {
            return label_map;
        }
    }
}

/// A single fixpoint step: walk every opcode, tracking the running byte
/// position, and update the map of `JUMPDEST` positions. Returns whether every
/// position was already aligned along with the updated map.
fn step(
    label_map: &HashMap<Label, Position>,
    opcodes: &[LabelledOpcode],
) -> (bool, HashMap<Label, Position>) {
    let mut done = true;
    let mut pos: Position = 0;
    let mut map = label_map.clone();
    for op in opcodes {
        align(&mut done, &mut pos, &mut map, op);
    }
    (done, map)
}

fn align(
    done: &mut bool,
    pos: &mut Position,
    map: &mut HashMap<Label, Position>,
    op: &LabelledOpcode,
) {
    if let Opcode::Jumpdest(label) = op {
        // A 'JUMPDEST' not seen before, or seen at another offset, is not yet
        // aligned; record it and signal that another iteration is necessary.
        let aligned = map.get(label).copied() == Some(*pos);
        *done = *done && aligned;
        map.insert(label.clone(), *pos);
        *pos += opcode_size(&jumpdest());
    } else if let Some(label) = op.jump_annot() {
        // If the destination was seen, add the size of a jump to it; otherwise
        // the offset is still approximate and another iteration is necessary.
        match map.get(label).copied() {
            Some(byte_pos) => *pos += jump_size(byte_pos),
            None => {
                *done = false;
                *pos += jump_size(0);
            }
        }
    } else {
        *pos += opcode_size(&op.concrete());
    }
}
