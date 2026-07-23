//! Exercises the labelled -> positional -> plain translation pipeline, using
//! the same example as the README (and the Haskell test-suite).

use evm_opcodes::labelled::{self, LabelledOpcode};
use evm_opcodes::positional::{self, PositionalOpcode};
use evm_opcodes::{Opcode, Ord16, PlainOpcode, U256, jump, jumpdest, jumpi};

fn push(n: u64) -> Opcode<()> {
    Opcode::Push(U256::from(n))
}

/// `int x = 1; while (x != 0) { x *= 2 }` compiled to labelled opcodes.
fn program() -> Vec<LabelledOpcode> {
    use Opcode::*;
    vec![
        Push(U256::from(1)),
        Jumpdest("loop".to_string()),
        Dup(Ord16::N1),
        Iszero,
        Jumpi("end".to_string()),
        Push(U256::from(2)),
        Mul,
        Jump("loop".to_string()),
        Jumpdest("end".to_string()),
    ]
}

#[test]
fn labelled_to_positional() {
    use Opcode::*;
    let expected: Vec<PositionalOpcode> = vec![
        Push(U256::from(1)),
        Jumpdest(2),
        Dup(Ord16::N1),
        Iszero,
        Jumpi(14),
        Push(U256::from(2)),
        Mul,
        Jump(2),
        Jumpdest(14),
    ];
    assert_eq!(labelled::translate(&program()), Ok(expected));
}

#[test]
fn positional_to_plain() {
    use Opcode::*;
    let positional = labelled::translate(&program()).unwrap();
    let expected: Vec<PlainOpcode> = vec![
        push(1),
        jumpdest(),
        Dup(Ord16::N1),
        Iszero,
        push(14),
        jumpi(),
        push(2),
        Mul,
        push(2),
        jump(),
        jumpdest(),
    ];
    assert_eq!(positional::translate(&positional), expected);
}

#[test]
fn missing_jumpdest_is_an_error() {
    let ops = vec![Opcode::Jump("nowhere".to_string())];
    let err = labelled::translate(&ops).unwrap_err();
    assert_eq!(err.missing_jumpdests, vec!["nowhere".to_string()]);
    assert!(err.duplicate_jumpdests.is_empty());
}

#[test]
fn duplicate_jumpdest_is_an_error() {
    let ops = vec![
        Opcode::Jumpdest("dup".to_string()),
        Opcode::Jumpdest("dup".to_string()),
    ];
    let err = labelled::translate(&ops).unwrap_err();
    assert_eq!(err.duplicate_jumpdests, vec!["dup".to_string()]);
}
