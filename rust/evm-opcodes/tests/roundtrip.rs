use std::collections::BTreeSet;

use evm_opcodes::{
    Opcode, Ord5, Ord16, PlainOpcode, U256, jump, jumpdest, jumpi, pack, read_op, to_bytes, to_hex,
};
use proptest::prelude::*;

/// Every single-byte opcode (i.e. everything except `PUSH` with a constant).
fn single_byte_opcodes() -> Vec<PlainOpcode> {
    use Opcode::*;
    let mut ops = vec![
        Stop,
        Add,
        Mul,
        Sub,
        Div,
        Sdiv,
        Mod,
        Smod,
        Addmod,
        Mulmod,
        Exp,
        Signextend,
        Lt,
        Gt,
        Slt,
        Sgt,
        Eq,
        Iszero,
        And,
        Or,
        Xor,
        Not,
        Byte,
        Shl,
        Shr,
        Sar,
        Clz,
        Keccak256,
        Address,
        Balance,
        Origin,
        Caller,
        Callvalue,
        Calldataload,
        Calldatasize,
        Calldatacopy,
        Codesize,
        Codecopy,
        Gasprice,
        Extcodesize,
        Extcodecopy,
        Returndatasize,
        Returndatacopy,
        Extcodehash,
        Blockhash,
        Coinbase,
        Timestamp,
        Number,
        Prevrandao,
        Gaslimit,
        Chainid,
        Selfbalance,
        Basefee,
        Blobhash,
        Blobbasefee,
        Pop,
        Mload,
        Mstore,
        Mstore8,
        Sload,
        Sstore,
        Pc,
        Msize,
        Gas,
        Tload,
        Tstore,
        Mcopy,
        Push0,
        Create,
        Call,
        Callcode,
        Return,
        Delegatecall,
        Create2,
        Staticcall,
        Revert,
        Invalid,
        Selfdestruct,
    ];
    ops.push(jump());
    ops.push(jumpi());
    ops.push(jumpdest());
    for n in 1..=16 {
        ops.push(Dup(Ord16::from_num(n).unwrap()));
        ops.push(Swap(Ord16::from_num(n).unwrap()));
    }
    for n in 0..=4 {
        ops.push(Log(Ord5::from_num(n).unwrap()));
    }
    ops
}

#[test]
fn single_byte_roundtrip() {
    for op in single_byte_opcodes() {
        let bytes = pack(std::slice::from_ref(&op));
        let (head, tail) = bytes.split_first().expect("non-empty encoding");
        assert_eq!(read_op(*head, tail), Some(op.clone()), "roundtrip for {op}");
    }
}

#[test]
fn read_op_coverage() {
    // The set of leading bytes that must decode, plus the whole PUSH range.
    let mut defined: BTreeSet<u8> = single_byte_opcodes()
        .iter()
        .map(|op| to_bytes(op)[0])
        .collect();
    defined.extend(0x60..=0x7f);

    let filler = [0u8; 32];
    for b in 0u8..=255 {
        let got = read_op(b, &filler);
        if defined.contains(&b) {
            assert!(got.is_some(), "expected {b:#04x} to decode");
        } else {
            assert!(got.is_none(), "expected {b:#04x} to be undefined");
        }
    }
}

#[test]
fn display_names() {
    assert_eq!(Opcode::<()>::Add.to_string(), "ADD");
    assert_eq!(Opcode::<()>::Clz.to_string(), "CLZ");
    assert_eq!(Opcode::<()>::Keccak256.to_string(), "KECCAK256");
    assert_eq!(Opcode::<()>::Selfdestruct.to_string(), "SELFDESTRUCT");
    assert_eq!(Opcode::<()>::Dup(Ord16::N1).to_string(), "DUP1");
    assert_eq!(Opcode::<()>::Swap(Ord16::N16).to_string(), "SWAP16");
    assert_eq!(Opcode::<()>::Log(Ord5::N0).to_string(), "LOG0");
    assert_eq!(Opcode::<()>::Push(U256::from(255)).to_string(), "PUSH 255");
    assert_eq!(Opcode::Jump("loop").to_string(), "JUMP loop");
}

/// Cross-language parity: this program's hex is produced by the Haskell
/// `evm-opcodes` `toHex` on the identical opcode list.
#[test]
fn hex_parity_with_haskell() {
    use Opcode::*;
    let program: Vec<PlainOpcode> = vec![
        Push0,
        Push(U256::from(255)),
        Push(U256::from(256)),
        Push(U256::from(66051)),
        Dup(Ord16::N1),
        Swap(Ord16::N16),
        Log(Ord5::N0),
        Clz,
        jump(),
        jumpi(),
        jumpdest(),
        Mcopy,
        Push(U256::from(0)),
        Stop,
    ];
    assert_eq!(
        to_hex(&program),
        "5f60ff61010062010203809fa01e56575b5e600000"
    );
}

proptest! {
    #[test]
    fn push_roundtrip(bytes in any::<[u8; 32]>()) {
        let n = U256::from_be_bytes::<32>(bytes);
        let op: PlainOpcode = Opcode::Push(n);
        let encoded = pack(std::slice::from_ref(&op));
        let (head, tail) = encoded.split_first().unwrap();
        prop_assert_eq!(read_op(*head, tail), Some(op));
    }
}
