//! Opcode types for the Ethereum Virtual Machine (EVM).
//!
//! This crate is a port of the `evm-opcodes` Haskell package. It exposes the
//! [`Opcode`] type for expressing EVM opcodes as extracted from the Ethereum
//! Yellow Paper (appendix H.2) with amendments from various EIPs. The Yellow
//! Paper is available at <https://ethereum.github.io/yellowpaper/paper.pdf>.
//!
//! The opcode list is not always up-to-date in the Yellow Paper, so keeping
//! track of EIPs that add or modify instructions is necessary. See the variant
//! comments for the references to these additions.

mod encode;
mod push;
mod spec;

pub mod labelled;
pub mod positional;
pub mod traversal;

pub use encode::{
    is_dup, is_log, is_push, is_swap, opcode_size, pack, read_dup, read_log, read_op, read_push,
    read_swap, to_bytes, to_hex,
};
pub use push::push_bytes;
pub use ruint::aliases::U256;
pub use spec::{JumpAnnotDisplay, OpcodeSpec, opcode_spec};

/// An [`Opcode`] is an EVM opcode with parameterised jumps.
///
/// The type parameter `J` is the annotation attached to `JUMP`, `JUMPI` and
/// `JUMPDEST`. For a plain opcode using basic stack-based jumps, use the
/// [`PlainOpcode`] alias (`Opcode<()>`).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Opcode<J> {
    // 0s: Stop and Arithmetic Operations
    Stop,       // 0x00
    Add,        // 0x01
    Mul,        // 0x02
    Sub,        // 0x03
    Div,        // 0x04
    Sdiv,       // 0x05
    Mod,        // 0x06
    Smod,       // 0x07
    Addmod,     // 0x08
    Mulmod,     // 0x09
    Exp,        // 0x0a
    Signextend, // 0x0b

    // 10s: Comparison & Bitwise Logic Operations
    Lt,     // 0x10
    Gt,     // 0x11
    Slt,    // 0x12
    Sgt,    // 0x13
    Eq,     // 0x14
    Iszero, // 0x15
    And,    // 0x16
    Or,     // 0x17
    Xor,    // 0x18
    Not,    // 0x19
    Byte,   // 0x1a
    Shl,    // 0x1b, https://eips.ethereum.org/EIPS/eip-145
    Shr,    // 0x1c, https://eips.ethereum.org/EIPS/eip-145
    Sar,    // 0x1d, https://eips.ethereum.org/EIPS/eip-145
    Clz,    // 0x1e, https://eips.ethereum.org/EIPS/eip-7939

    // 20s: KECCAK256
    Keccak256, // 0x20, https://eips.ethereum.org/EIPS/eip-1803

    // 30s: Environmental Information
    Address,        // 0x30
    Balance,        // 0x31
    Origin,         // 0x32
    Caller,         // 0x33
    Callvalue,      // 0x34
    Calldataload,   // 0x35
    Calldatasize,   // 0x36
    Calldatacopy,   // 0x37
    Codesize,       // 0x38
    Codecopy,       // 0x39
    Gasprice,       // 0x3a
    Extcodesize,    // 0x3b
    Extcodecopy,    // 0x3c
    Returndatasize, // 0x3d, https://eips.ethereum.org/EIPS/eip-211
    Returndatacopy, // 0x3e, https://eips.ethereum.org/EIPS/eip-211
    Extcodehash,    // 0x3f, https://eips.ethereum.org/EIPS/eip-1052

    // 40s: Block Information
    Blockhash,   // 0x40
    Coinbase,    // 0x41
    Timestamp,   // 0x42
    Number,      // 0x43
    Prevrandao,  // 0x44, https://eips.ethereum.org/EIPS/eip-4399
    Gaslimit,    // 0x45
    Chainid,     // 0x46, https://eips.ethereum.org/EIPS/eip-1344
    Selfbalance, // 0x47, https://eips.ethereum.org/EIPS/eip-1884
    Basefee,     // 0x48, https://eips.ethereum.org/EIPS/eip-3198
    Blobhash,    // 0x49, https://eips.ethereum.org/EIPS/eip-4844
    Blobbasefee, // 0x4a, https://eips.ethereum.org/EIPS/eip-7516

    // 50s: Stack, Memory, Storage and Flow Operations
    Pop,         // 0x50
    Mload,       // 0x51
    Mstore,      // 0x52
    Mstore8,     // 0x53
    Sload,       // 0x54
    Sstore,      // 0x55
    Jump(J),     // 0x56
    Jumpi(J),    // 0x57
    Pc,          // 0x58
    Msize,       // 0x59
    Gas,         // 0x5a
    Jumpdest(J), // 0x5b
    Tload,       // 0x5c, https://eips.ethereum.org/EIPS/eip-1153
    Tstore,      // 0x5d, https://eips.ethereum.org/EIPS/eip-1153
    Mcopy,       // 0x5e, https://eips.ethereum.org/EIPS/eip-5656

    // 5f, 60s & 70s: Push Operations
    Push0,      // 0x5f, https://eips.ethereum.org/EIPS/eip-3855
    Push(U256), // 0x60 - 0x7f (PUSH1 - PUSH32)

    // 80s: Duplication Operations (DUP1 - DUP16, 0x80 - 0x8f)
    Dup(Ord16),
    // 90s: Exchange operations (SWAP1 - SWAP16, 0x90 - 0x9f)
    Swap(Ord16),
    // a0s: Logging Operations (LOG0 - LOG4, 0xa0 - 0xa4)
    Log(Ord5),

    // f0s: System Operations
    Create,       // 0xf0
    Call,         // 0xf1
    Callcode,     // 0xf2
    Return,       // 0xf3
    Delegatecall, // 0xf4, https://eips.ethereum.org/EIPS/eip-7
    Create2,      // 0xf5, https://eips.ethereum.org/EIPS/eip-1014
    Staticcall,   // 0xfa
    Revert,       // 0xfd, https://eips.ethereum.org/EIPS/eip-140
    Invalid,      // 0xfe, https://eips.ethereum.org/EIPS/eip-141
    Selfdestruct, // 0xff, https://eips.ethereum.org/EIPS/eip-6
}

/// A plain, parameterless EVM opcode (basic stack-based jumps).
pub type PlainOpcode = Opcode<()>;

/// A value of cardinality 5 for `LOG` (`LOG0` - `LOG4`). The numeric value is
/// the log number (0 - 4).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Ord5 {
    N0 = 0,
    N1,
    N2,
    N3,
    N4,
}

impl Ord5 {
    /// The log number (0 - 4).
    pub const fn num(self) -> u8 {
        self as u8
    }

    /// Build an [`Ord5`] from a log number (0 - 4).
    pub fn from_num(n: u8) -> Option<Self> {
        use Ord5::*;
        Some(match n {
            0 => N0,
            1 => N1,
            2 => N2,
            3 => N3,
            4 => N4,
            _ => return None,
        })
    }
}

/// A value of cardinality 16 for `DUP` and `SWAP`. The numeric value is the
/// dup/swap number (1 - 16).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Ord16 {
    N1 = 1,
    N2,
    N3,
    N4,
    N5,
    N6,
    N7,
    N8,
    N9,
    N10,
    N11,
    N12,
    N13,
    N14,
    N15,
    N16,
}

impl Ord16 {
    /// The dup/swap number (1 - 16).
    pub const fn num(self) -> u8 {
        self as u8
    }

    /// Build an [`Ord16`] from a dup/swap number (1 - 16).
    pub fn from_num(n: u8) -> Option<Self> {
        use Ord16::*;
        Some(match n {
            1 => N1,
            2 => N2,
            3 => N3,
            4 => N4,
            5 => N5,
            6 => N6,
            7 => N7,
            8 => N8,
            9 => N9,
            10 => N10,
            11 => N11,
            12 => N12,
            13 => N13,
            14 => N14,
            15 => N15,
            16 => N16,
            _ => return None,
        })
    }
}

/// A plain parameterless `JUMP`.
pub fn jump() -> PlainOpcode {
    Opcode::Jump(())
}

/// A plain parameterless `JUMPI`.
pub fn jumpi() -> PlainOpcode {
    Opcode::Jumpi(())
}

/// A plain parameterless `JUMPDEST`.
pub fn jumpdest() -> PlainOpcode {
    Opcode::Jumpdest(())
}

impl<J> Opcode<J> {
    /// Map the jump annotation, turning an `Opcode<J>` into an `Opcode<K>`.
    ///
    /// This is the `Functor` instance of the Haskell `Opcode'` type: `f` is
    /// applied to the annotation of `JUMP`, `JUMPI` and `JUMPDEST`; every other
    /// opcode is carried over unchanged.
    pub fn map_annot<K>(self, f: impl FnOnce(J) -> K) -> Opcode<K> {
        use Opcode::*;
        // Only the three jump variants carry `J`; the rest are rebuilt as-is.
        macro_rules! rebuild {
            ($($v:ident),+ $(,)?) => {
                match self {
                    Jump(j) => Jump(f(j)),
                    Jumpi(j) => Jumpi(f(j)),
                    Jumpdest(j) => Jumpdest(f(j)),
                    Push(n) => Push(n),
                    Push0 => Push0,
                    Dup(i) => Dup(i),
                    Swap(i) => Swap(i),
                    Log(i) => Log(i),
                    $( $v => $v, )+
                }
            };
        }
        rebuild!(
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
            Create,
            Call,
            Callcode,
            Return,
            Delegatecall,
            Create2,
            Staticcall,
            Revert,
            Invalid,
            Selfdestruct
        )
    }

    /// Convert any `Opcode<J>` into a [`PlainOpcode`] (`Opcode<()>`).
    pub fn concrete(&self) -> PlainOpcode
    where
        J: Clone,
    {
        self.clone().map_annot(|_| ())
    }

    /// Extract the annotation from a `JUMP` or a `JUMPI`.
    pub fn jump_annot(&self) -> Option<&J> {
        match self {
            Opcode::Jump(j) | Opcode::Jumpi(j) => Some(j),
            _ => None,
        }
    }

    /// Extract the annotation from a `JUMPDEST`.
    pub fn jumpdest_annot(&self) -> Option<&J> {
        match self {
            Opcode::Jumpdest(j) => Some(j),
            _ => None,
        }
    }
}
