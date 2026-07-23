use core::fmt;

use crate::Opcode;
use crate::push::push_bytes;

/// An [`OpcodeSpec`] for a given [`Opcode`] contains the numeric encoding of
/// the opcode, the number of items it places on the stack (α), the number of
/// items it removes from the stack (δ), and its printable name. These values
/// are documented in the Ethereum Yellow Paper.
///
/// Examples:
///
/// ```text
///            Hex  α δ
/// OpcodeSpec 0x01 2 1 "add"
/// OpcodeSpec 0x60 0 1 "push1 255"
/// OpcodeSpec 0x61 0 1 "push2 256"
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpcodeSpec {
    /// Numeric encoding of the opcode.
    pub encoding: u8,
    /// Number of items the opcode places on the stack (α).
    pub alpha: u8,
    /// Number of items the opcode removes from the stack (δ).
    pub delta: u8,
    /// Printable name for the opcode, e.g. `"add"`.
    pub name: String,
}

impl OpcodeSpec {
    fn new(encoding: u8, alpha: u8, delta: u8, name: &str) -> OpcodeSpec {
        OpcodeSpec {
            encoding,
            alpha,
            delta,
            name: name.to_string(),
        }
    }
}

/// Given an [`Opcode`], produce its [`OpcodeSpec`].
///
/// For `DUP`, `SWAP` and `LOG` this depends on the specific variant, and for
/// `PUSH` it depends on the size of the constant being pushed.
pub fn opcode_spec<J>(opcode: &Opcode<J>) -> OpcodeSpec {
    use Opcode::*;
    match opcode {
        // 0s: Stop and Arithmetic Operations
        Stop => OpcodeSpec::new(0x00, 0, 0, "stop"),
        Add => OpcodeSpec::new(0x01, 2, 1, "add"),
        Mul => OpcodeSpec::new(0x02, 2, 1, "mul"),
        Sub => OpcodeSpec::new(0x03, 2, 1, "sub"),
        Div => OpcodeSpec::new(0x04, 2, 1, "div"),
        Sdiv => OpcodeSpec::new(0x05, 2, 1, "sdiv"),
        Mod => OpcodeSpec::new(0x06, 2, 1, "mod"),
        Smod => OpcodeSpec::new(0x07, 2, 1, "smod"),
        Addmod => OpcodeSpec::new(0x08, 3, 1, "addmod"),
        Mulmod => OpcodeSpec::new(0x09, 3, 1, "mulmod"),
        Exp => OpcodeSpec::new(0x0a, 2, 1, "exp"),
        Signextend => OpcodeSpec::new(0x0b, 2, 1, "signextend"),

        // 10s: Comparison & Bitwise Logic Operations
        Lt => OpcodeSpec::new(0x10, 2, 1, "lt"),
        Gt => OpcodeSpec::new(0x11, 2, 1, "gt"),
        Slt => OpcodeSpec::new(0x12, 2, 1, "slt"),
        Sgt => OpcodeSpec::new(0x13, 2, 1, "sgt"),
        Eq => OpcodeSpec::new(0x14, 2, 1, "eq"),
        Iszero => OpcodeSpec::new(0x15, 1, 1, "iszero"),
        And => OpcodeSpec::new(0x16, 2, 1, "and"),
        Or => OpcodeSpec::new(0x17, 2, 1, "or"),
        Xor => OpcodeSpec::new(0x18, 2, 1, "xor"),
        Not => OpcodeSpec::new(0x19, 1, 1, "not"),
        Byte => OpcodeSpec::new(0x1a, 2, 1, "byte"),
        Shl => OpcodeSpec::new(0x1b, 2, 1, "shl"),
        Shr => OpcodeSpec::new(0x1c, 2, 1, "shr"),
        Sar => OpcodeSpec::new(0x1d, 2, 1, "sar"),
        Clz => OpcodeSpec::new(0x1e, 1, 1, "clz"),

        // 20s: KECCAK256
        Keccak256 => OpcodeSpec::new(0x20, 2, 1, "keccak256"),

        // 30s: Environmental Information
        Address => OpcodeSpec::new(0x30, 0, 1, "address"),
        Balance => OpcodeSpec::new(0x31, 1, 1, "balance"),
        Origin => OpcodeSpec::new(0x32, 0, 1, "origin"),
        Caller => OpcodeSpec::new(0x33, 0, 1, "caller"),
        Callvalue => OpcodeSpec::new(0x34, 0, 1, "callvalue"),
        Calldataload => OpcodeSpec::new(0x35, 1, 1, "calldataload"),
        Calldatasize => OpcodeSpec::new(0x36, 0, 1, "calldatasize"),
        Calldatacopy => OpcodeSpec::new(0x37, 3, 0, "calldatacopy"),
        Codesize => OpcodeSpec::new(0x38, 0, 1, "codesize"),
        Codecopy => OpcodeSpec::new(0x39, 3, 0, "codecopy"),
        Gasprice => OpcodeSpec::new(0x3a, 0, 1, "gasprice"),
        Extcodesize => OpcodeSpec::new(0x3b, 1, 1, "extcodesize"),
        Extcodecopy => OpcodeSpec::new(0x3c, 4, 0, "extcodecopy"),
        Returndatasize => OpcodeSpec::new(0x3d, 0, 1, "returndatasize"),
        Returndatacopy => OpcodeSpec::new(0x3e, 3, 0, "returndatacopy"),
        Extcodehash => OpcodeSpec::new(0x3f, 1, 1, "extcodehash"),

        // 40s: Block Information
        Blockhash => OpcodeSpec::new(0x40, 1, 1, "blockhash"),
        Coinbase => OpcodeSpec::new(0x41, 0, 1, "coinbase"),
        Timestamp => OpcodeSpec::new(0x42, 0, 1, "timestamp"),
        Number => OpcodeSpec::new(0x43, 0, 1, "number"),
        Prevrandao => OpcodeSpec::new(0x44, 0, 1, "prevrandao"),
        Gaslimit => OpcodeSpec::new(0x45, 0, 1, "gaslimit"),
        Chainid => OpcodeSpec::new(0x46, 0, 1, "chainid"),
        Selfbalance => OpcodeSpec::new(0x47, 0, 1, "selfbalance"),
        Basefee => OpcodeSpec::new(0x48, 0, 1, "basefee"),
        Blobhash => OpcodeSpec::new(0x49, 1, 1, "blobhash"),
        Blobbasefee => OpcodeSpec::new(0x4a, 0, 1, "blobbasefee"),

        // 50s: Stack, Memory, Storage and Flow Operations
        Pop => OpcodeSpec::new(0x50, 1, 0, "pop"),
        Mload => OpcodeSpec::new(0x51, 1, 1, "mload"),
        Mstore => OpcodeSpec::new(0x52, 2, 0, "mstore"),
        Mstore8 => OpcodeSpec::new(0x53, 2, 0, "mstore8"),
        Sload => OpcodeSpec::new(0x54, 1, 1, "sload"),
        Sstore => OpcodeSpec::new(0x55, 2, 0, "sstore"),
        Jump(_) => OpcodeSpec::new(0x56, 1, 0, "jump"),
        Jumpi(_) => OpcodeSpec::new(0x57, 2, 0, "jumpi"),
        Pc => OpcodeSpec::new(0x58, 0, 1, "pc"),
        Msize => OpcodeSpec::new(0x59, 0, 1, "msize"),
        Gas => OpcodeSpec::new(0x5a, 0, 1, "gas"),
        Jumpdest(_) => OpcodeSpec::new(0x5b, 0, 0, "jumpdest"),
        Tload => OpcodeSpec::new(0x5c, 1, 1, "tload"),
        Tstore => OpcodeSpec::new(0x5d, 2, 0, "tstore"),
        Mcopy => OpcodeSpec::new(0x5e, 3, 0, "mcopy"),

        // 5f, 60s & 70s: Push Operations
        Push0 => OpcodeSpec::new(0x5f, 0, 1, "push0"),
        Push(n) => {
            let (push_hex, push_const) = push_bytes(*n);
            OpcodeSpec {
                encoding: push_hex,
                alpha: 0,
                delta: 1,
                name: format!("push{} {}", push_const.len(), n),
            }
        }

        // 80s: Duplication Operations (DUP)
        Dup(i) => {
            let num = i.num();
            OpcodeSpec {
                encoding: 0x80 + num - 1,
                alpha: num,
                delta: num + 1,
                name: format!("dup{num}"),
            }
        }

        // 90s: Exchange operations (SWAP)
        Swap(i) => {
            let num = i.num();
            OpcodeSpec {
                encoding: 0x90 + num - 1,
                alpha: num,
                delta: num,
                name: format!("swap{num}"),
            }
        }

        // a0s: Logging Operations (LOG)
        Log(i) => {
            let num = i.num();
            OpcodeSpec {
                encoding: 0xa0 + num,
                alpha: num + 2,
                delta: 0,
                name: format!("log{num}"),
            }
        }

        // f0s: System Operations
        Create => OpcodeSpec::new(0xf0, 3, 1, "create"),
        Call => OpcodeSpec::new(0xf1, 7, 1, "call"),
        Callcode => OpcodeSpec::new(0xf2, 7, 1, "callcode"),
        Return => OpcodeSpec::new(0xf3, 2, 0, "return"),
        Delegatecall => OpcodeSpec::new(0xf4, 6, 1, "delegatecall"),
        Create2 => OpcodeSpec::new(0xf5, 4, 1, "create2"),
        Staticcall => OpcodeSpec::new(0xfa, 6, 1, "staticcall"),
        Revert => OpcodeSpec::new(0xfd, 2, 0, "revert"),
        Invalid => OpcodeSpec::new(0xfe, 0, 0, "invalid"),
        Selfdestruct => OpcodeSpec::new(0xff, 1, 0, "selfdestruct"),
    }
}

/// How a jump annotation is rendered by the [`Opcode`] [`fmt::Display`] impl.
///
/// The plain annotation `()` renders nothing, so `Opcode::<()>::Jump(())`
/// displays as `JUMP`; an annotated jump such as `Opcode::Jump("loop")`
/// displays as `JUMP loop`.
pub trait JumpAnnotDisplay {
    /// Render this annotation, including any leading separator.
    fn fmt_jump_annot(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl JumpAnnotDisplay for () {
    fn fmt_jump_annot(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl JumpAnnotDisplay for u64 {
    fn fmt_jump_annot(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, " {self}")
    }
}

impl JumpAnnotDisplay for String {
    fn fmt_jump_annot(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, " {self}")
    }
}

impl JumpAnnotDisplay for &str {
    fn fmt_jump_annot(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, " {self}")
    }
}

impl<J: JumpAnnotDisplay> fmt::Display for Opcode<J> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Opcode::Push(n) => write!(f, "PUSH {n}"),
            Opcode::Jump(j) => {
                write!(f, "JUMP")?;
                j.fmt_jump_annot(f)
            }
            Opcode::Jumpi(j) => {
                write!(f, "JUMPI")?;
                j.fmt_jump_annot(f)
            }
            Opcode::Jumpdest(j) => {
                write!(f, "JUMPDEST")?;
                j.fmt_jump_annot(f)
            }
            _ => write!(f, "{}", opcode_spec(self).name.to_uppercase()),
        }
    }
}
