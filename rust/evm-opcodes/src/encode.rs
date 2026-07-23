use crate::push::push_bytes;
use crate::spec::opcode_spec;
use crate::{Opcode, Ord5, Ord16, PlainOpcode, U256, jump, jumpdest, jumpi};

/// Determine if a byte represents a `DUP` opcode (`DUP1` - `DUP16`).
pub fn is_dup(b: u8) -> bool {
    read_dup(b).is_some()
}

/// Determine if a byte represents a `SWAP` opcode (`SWAP1` - `SWAP16`).
pub fn is_swap(b: u8) -> bool {
    read_swap(b).is_some()
}

/// Determine if a byte represents a `LOG` opcode (`LOG0` - `LOG4`).
pub fn is_log(b: u8) -> bool {
    read_log(b).is_some()
}

/// Determine if a byte represents a `PUSH` opcode.
pub fn is_push(b: u8, bs: &[u8]) -> bool {
    read_push(b, bs).is_some()
}

/// Read a `DUP` opcode (`DUP1` - `DUP16`) safely.
pub fn read_dup(b: u8) -> Option<PlainOpcode> {
    if (0x80..=0x8f).contains(&b) {
        Ord16::from_num(b - 0x80 + 1).map(Opcode::Dup)
    } else {
        None
    }
}

/// Read a `SWAP` opcode (`SWAP1` - `SWAP16`) safely.
pub fn read_swap(b: u8) -> Option<PlainOpcode> {
    if (0x90..=0x9f).contains(&b) {
        Ord16::from_num(b - 0x90 + 1).map(Opcode::Swap)
    } else {
        None
    }
}

/// Read a `LOG` opcode (`LOG0` - `LOG4`) safely.
pub fn read_log(b: u8) -> Option<PlainOpcode> {
    if (0xa0..=0xa4).contains(&b) {
        Ord5::from_num(b - 0xa0).map(Opcode::Log)
    } else {
        None
    }
}

/// Read a `PUSH` opcode safely, reading the pushed constant from `bs`.
///
/// If fewer than the required number of bytes are available in `bs`, the
/// constant is read from what is present (left-padded with zeros), matching
/// the Haskell `readPUSH`.
pub fn read_push(b: u8, bs: &[u8]) -> Option<PlainOpcode> {
    if !(0x60..=0x7f).contains(&b) {
        return None;
    }
    let n = (b - 0x60 + 1) as usize;
    let take = n.min(bs.len());
    Some(Opcode::Push(word256(&bs[..take])))
}

/// Parse an [`Opcode`] from a byte. For `PUSH` instructions, the pushed
/// constant is read from the subsequent bytes `bs`.
pub fn read_op(word: u8, bs: &[u8]) -> Option<PlainOpcode> {
    if let Some(op) = read_dup(word)
        .or_else(|| read_swap(word))
        .or_else(|| read_log(word))
        .or_else(|| read_push(word, bs))
    {
        return Some(op);
    }

    use Opcode::*;
    Some(match word {
        // 0s: Stop and Arithmetic Operations
        0x00 => Stop,
        0x01 => Add,
        0x02 => Mul,
        0x03 => Sub,
        0x04 => Div,
        0x05 => Sdiv,
        0x06 => Mod,
        0x07 => Smod,
        0x08 => Addmod,
        0x09 => Mulmod,
        0x0a => Exp,
        0x0b => Signextend,

        // 10s: Comparison & Bitwise Logic Operations
        0x10 => Lt,
        0x11 => Gt,
        0x12 => Slt,
        0x13 => Sgt,
        0x14 => Eq,
        0x15 => Iszero,
        0x16 => And,
        0x17 => Or,
        0x18 => Xor,
        0x19 => Not,
        0x1a => Byte,
        0x1b => Shl,
        0x1c => Shr,
        0x1d => Sar,
        0x1e => Clz,

        // 20s: KECCAK256
        0x20 => Keccak256,

        // 30s: Environmental Information
        0x30 => Address,
        0x31 => Balance,
        0x32 => Origin,
        0x33 => Caller,
        0x34 => Callvalue,
        0x35 => Calldataload,
        0x36 => Calldatasize,
        0x37 => Calldatacopy,
        0x38 => Codesize,
        0x39 => Codecopy,
        0x3a => Gasprice,
        0x3b => Extcodesize,
        0x3c => Extcodecopy,
        0x3d => Returndatasize,
        0x3e => Returndatacopy,
        0x3f => Extcodehash,

        // 40s: Block Information
        0x40 => Blockhash,
        0x41 => Coinbase,
        0x42 => Timestamp,
        0x43 => Number,
        0x44 => Prevrandao,
        0x45 => Gaslimit,
        0x46 => Chainid,
        0x47 => Selfbalance,
        0x48 => Basefee,
        0x49 => Blobhash,
        0x4a => Blobbasefee,

        // 50s: Stack, Memory, Storage and Flow Operations
        0x50 => Pop,
        0x51 => Mload,
        0x52 => Mstore,
        0x53 => Mstore8,
        0x54 => Sload,
        0x55 => Sstore,
        0x56 => jump(),
        0x57 => jumpi(),
        0x58 => Pc,
        0x59 => Msize,
        0x5a => Gas,
        0x5b => jumpdest(),
        0x5c => Tload,
        0x5d => Tstore,
        0x5e => Mcopy,
        0x5f => Push0,

        // f0s: System Operations
        0xf0 => Create,
        0xf1 => Call,
        0xf2 => Callcode,
        0xf3 => Return,
        0xf4 => Delegatecall,
        0xf5 => Create2,
        0xfa => Staticcall,
        0xfd => Revert,
        0xfe => Invalid,
        0xff => Selfdestruct,

        // Unknown
        _ => return None,
    })
}

/// Read a [`U256`] from a big-endian byte slice of at most 32 bytes,
/// left-padding with zeros. Matches the Haskell `word256`.
fn word256(bs: &[u8]) -> U256 {
    U256::from_be_slice(bs)
}

/// Calculate the size in bytes of an encoded opcode. The only opcode that uses
/// more than one byte is `PUSH`.
pub fn opcode_size(opcode: &PlainOpcode) -> u64 {
    match opcode {
        Opcode::Push(n) => {
            let (_, push_const) = push_bytes(*n);
            1 + push_const.len() as u64
        }
        _ => 1,
    }
}

/// Convert an [`Opcode`] to its byte encoding. To convert many opcodes to
/// bytecode, use [`pack`].
pub fn to_bytes(opcode: &PlainOpcode) -> Vec<u8> {
    match opcode {
        Opcode::Push(n) => {
            let (push_hex, mut push_const) = push_bytes(*n);
            let mut bytes = Vec::with_capacity(1 + push_const.len());
            bytes.push(push_hex);
            bytes.append(&mut push_const);
            bytes
        }
        _ => vec![opcode_spec(opcode).encoding],
    }
}

/// Convert a slice of opcodes to bytecode.
pub fn pack(opcodes: &[PlainOpcode]) -> Vec<u8> {
    opcodes.iter().flat_map(to_bytes).collect()
}

/// Convert a slice of opcodes to a string of ASCII hexadecimals.
pub fn to_hex(opcodes: &[PlainOpcode]) -> String {
    pack(opcodes).iter().map(|b| format!("{b:02x}")).collect()
}
