use crate::U256;

/// Convert the constant argument of a `PUSH` to the opcode encoding
/// (0x60 - 0x7f) and its constant split into big-endian byte segments.
///
/// The constant is encoded with the minimal number of bytes (at least one, so
/// that `PUSH 0` becomes `PUSH1 0x00`), matching the Haskell `push'` function.
pub fn push_bytes(n: U256) -> (u8, Vec<u8>) {
    let be = n.to_be_bytes::<32>();
    // Drop leading zero bytes, but keep at least the least-significant byte.
    let first = be.iter().position(|&b| b != 0).unwrap_or(31);
    let bytes = be[first..].to_vec();
    let opcode = 0x60 + (bytes.len() as u8 - 1);
    (opcode, bytes)
}
