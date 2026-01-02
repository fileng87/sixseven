use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::HashMap;
use std::io::Read;
use std::sync::OnceLock;

/// Static cache for BigInt::one() to avoid repeated allocations in hot paths.
static BIGINT_ONE: OnceLock<BigInt> = OnceLock::new();

/// Returns a cached reference to BigInt::one().
/// This avoids creating a new BigInt instance on every Inc/Dec operation.
fn bigint_one() -> &'static BigInt {
    BIGINT_ONE.get_or_init(BigInt::one)
}

/// Tokens in the sixseven language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    /// Increment current cell by 1 (🫱)
    Inc,
    /// Decrement current cell by 1 (🫲)
    Dec,
    /// Output current cell as Unicode code point (🤷)
    Out,
    /// Start of number block or control block (67)
    SixSeven,
}

/// Errors that can occur during parsing or execution.
#[derive(Debug)]
pub enum Error {
    /// A number block (67...🤷) was not properly terminated.
    UnterminatedNumberBlock { token_index: usize },
    /// A number block contained an illegal token (67).
    IllegalTokenInNumberBlock { token_index: usize },
    /// A control block (67 67...🤷) was not properly terminated.
    UnterminatedControlBlock { token_index: usize },
    /// An unexpected token was encountered.
    UnexpectedToken { token_index: usize },
    /// Attempted to output an invalid Unicode code point.
    InvalidCodePoint { value: BigInt },
    /// I/O error occurred while reading input.
    Io(std::io::Error),
    /// Input contained invalid UTF-8.
    InvalidUtf8Input,
}

/// Tokenizes the input string into a sequence of tokens.
/// Non-instruction characters are treated as comments and ignored.
pub fn tokenize(input: &str) -> Result<Vec<Token>, Error> {
    // Pre-allocate with estimated capacity (most chars are comments, so reserve less)
    let mut out = Vec::with_capacity(input.len() / 4);
    let mut it = input.chars().peekable();

    while let Some(ch) = it.next() {
        match ch {
            '🫱' => out.push(Token::Inc),
            '🫲' => out.push(Token::Dec),
            '🤷' => out.push(Token::Out),
            '6' => match it.peek() {
                Some(&'7') => {
                    it.next();
                    out.push(Token::SixSeven);
                }
                _ => {
                    // Non-instruction: treat as comment (Brainfuck-style).
                }
            },
            _ => {
                // Non-instruction: treat as comment (Brainfuck-style).
            }
        }
    }

    Ok(out)
}

/// The execution machine with an infinite tape of integer cells.
#[derive(Debug, Default)]
struct Machine {
    /// Current data pointer position.
    ptr: i64,
    /// Sparse tape: only non-zero cells are stored in the HashMap.
    tape: HashMap<i64, BigInt>,
}

impl Machine {
    /// Returns a mutable reference to the current cell, initializing it to zero if needed.
    fn cell_mut(&mut self) -> &mut BigInt {
        self.tape.entry(self.ptr).or_insert_with(BigInt::zero)
    }

    /// Checks if the current cell is zero without cloning the value.
    /// This is optimized for the hot path in While loops.
    fn cell_is_zero(&self) -> bool {
        self.tape.get(&self.ptr).is_none_or(|v| v.is_zero())
    }
}

/// Executes source code with empty input.
pub fn run_source(source: &str) -> Result<String, Error> {
    let tokens = tokenize(source)?;
    run_tokens(&tokens)
}

/// Executes tokens with empty input.
pub fn run_tokens(tokens: &[Token]) -> Result<String, Error> {
    let mut input = std::io::empty();
    run_tokens_with_input(tokens, &mut input)
}

/// Executes source code with the given input stream.
pub fn run_source_with_input(source: &str, input: &mut dyn Read) -> Result<String, Error> {
    let tokens = tokenize(source)?;
    run_tokens_with_input(&tokens, input)
}

/// Executes tokens with the given input stream.
pub fn run_tokens_with_input(tokens: &[Token], input: &mut dyn Read) -> Result<String, Error> {
    let program = parse_program_m0(tokens)?;
    let mut m = Machine::default();
    // Pre-allocate output string with estimated capacity
    let mut out = String::with_capacity(tokens.len() / 2);
    exec_program(&program, &mut m, &mut out, input)?;
    Ok(out)
}

/// Reads a single Unicode scalar value from the input stream.
/// Returns None on EOF, or an error if the input is invalid UTF-8.
fn read_one_char(input: &mut dyn Read) -> Result<Option<char>, Error> {
    let mut buf = Vec::<u8>::with_capacity(4); // UTF-8 char max length
    let mut byte = [0u8; 1];

    loop {
        match input.read(&mut byte) {
            Ok(0) => {
                if buf.is_empty() {
                    return Ok(None); // EOF
                }
                return Err(Error::InvalidUtf8Input);
            }
            Ok(_) => {
                buf.push(byte[0]);
                match std::str::from_utf8(&buf) {
                    Ok(s) => {
                        let mut it = s.chars();
                        let ch = it.next().ok_or(Error::InvalidUtf8Input)?;
                        if it.next().is_some() {
                            // We somehow decoded more than one scalar from a single buffer.
                            // This shouldn't happen because buf is minimal for a single char,
                            // but keep it conservative.
                            return Err(Error::InvalidUtf8Input);
                        }
                        return Ok(Some(ch));
                    }
                    Err(e) => {
                        if e.error_len().is_some() {
                            return Err(Error::InvalidUtf8Input);
                        }
                        // else: need more bytes
                    }
                }
                if buf.len() > 4 {
                    return Err(Error::InvalidUtf8Input);
                }
            }
            Err(e) => return Err(Error::Io(e)),
        }
    }
}

/// Abstract syntax tree nodes representing program statements.
#[derive(Debug, Clone)]
enum Stmt {
    /// Increment current cell by 1.
    Inc,
    /// Decrement current cell by 1.
    Dec,
    /// Output current cell as Unicode code point.
    Out,
    /// Read one character from input (67🤷 with empty bit sequence).
    InputChar,
    /// Move pointer right by 1 (67🫱🤷, N==1).
    MoveRight,
    /// Move pointer left by 1 (67🫲🤷, N==0 with at least one bit).
    MoveLeft,
    /// Add the given value to current cell.
    Add(BigInt),
    /// While loop: execute body while current cell is non-zero.
    While(Vec<Stmt>),
}

/// Parses a sequence of tokens into a program (M0 mode: top-level statements).
fn parse_program_m0(tokens: &[Token]) -> Result<Vec<Stmt>, Error> {
    let mut i = 0usize;
    // Pre-allocate with estimated capacity (most tokens become statements)
    let mut out = Vec::with_capacity(tokens.len());
    while i < tokens.len() {
        out.push(parse_stmt_m0(tokens, &mut i)?);
    }
    Ok(out)
}

/// Parses a single statement in M0 mode (top-level).
fn parse_stmt_m0(tokens: &[Token], i: &mut usize) -> Result<Stmt, Error> {
    let Some(tok) = tokens.get(*i).copied() else {
        return Err(Error::UnexpectedToken { token_index: *i });
    };
    match tok {
        Token::Inc => {
            *i += 1;
            Ok(Stmt::Inc)
        }
        Token::Dec => {
            *i += 1;
            Ok(Stmt::Dec)
        }
        Token::Out => {
            *i += 1;
            Ok(Stmt::Out)
        }
        Token::SixSeven => {
            if tokens.get(*i + 1) == Some(&Token::SixSeven) {
                // Control block: 67 67 ... 🤷 (in M2: single 🤷 closes, 🤷🤷 encodes literal output)
                *i += 2;
                let body = parse_block_m2(tokens, i)?;
                Ok(Stmt::While(body))
            } else {
                // Number block / input / pointer move
                parse_number_like(tokens, i)
            }
        }
    }
}

/// Parses a single statement in M2 mode (inside control blocks).
fn parse_stmt_m2(tokens: &[Token], i: &mut usize) -> Result<Stmt, Error> {
    let Some(tok) = tokens.get(*i).copied() else {
        return Err(Error::UnterminatedControlBlock {
            token_index: i.saturating_sub(1),
        });
    };
    match tok {
        Token::Out => {
            // In M2, a single 🤷 closes the current block. A literal output inside the body must be escaped as 🤷🤷.
            if tokens.get(*i + 1) == Some(&Token::Out) {
                *i += 2;
                Ok(Stmt::Out)
            } else {
                // Caller must handle terminator.
                Err(Error::UnexpectedToken { token_index: *i })
            }
        }
        Token::Inc => {
            *i += 1;
            Ok(Stmt::Inc)
        }
        Token::Dec => {
            *i += 1;
            Ok(Stmt::Dec)
        }
        Token::SixSeven => {
            if tokens.get(*i + 1) == Some(&Token::SixSeven) {
                *i += 2;
                let body = parse_block_m2(tokens, i)?;
                Ok(Stmt::While(body))
            } else {
                parse_number_like(tokens, i)
            }
        }
    }
}

/// Parses a control block body (M2 mode).
/// A single 🤷 terminates the block; 🤷🤷 is an escaped output instruction.
fn parse_block_m2(tokens: &[Token], i: &mut usize) -> Result<Vec<Stmt>, Error> {
    // Pre-allocate with estimated capacity based on remaining tokens
    let remaining = tokens.len().saturating_sub(*i);
    let mut body = Vec::with_capacity(remaining.min(64)); // Cap at 64 to avoid over-allocation
    loop {
        let Some(tok) = tokens.get(*i).copied() else {
            return Err(Error::UnterminatedControlBlock {
                token_index: i.saturating_sub(1),
            });
        };
        match tok {
            Token::Out => {
                if tokens.get(*i + 1) == Some(&Token::Out) {
                    // escaped output
                    body.push(Stmt::Out);
                    *i += 2;
                    continue;
                }
                // terminator
                *i += 1;
                return Ok(body);
            }
            _ => body.push(parse_stmt_m2(tokens, i)?),
        }
    }
}

/// Parses a number block: 67 [bit_seq] 🤷
/// Special cases:
/// - Empty bit sequence (67🤷): InputChar
/// - N==0 with bits: MoveLeft
/// - N==1: MoveRight
/// - Otherwise: Add(N)
fn parse_number_like(tokens: &[Token], i: &mut usize) -> Result<Stmt, Error> {
    // Expects current token is 67
    let start = *i;
    *i += 1; // Consume 67
    let mut n = BigInt::zero();
    let mut bits = 0usize;
    while *i < tokens.len() {
        match tokens[*i] {
            Token::Inc => {
                n <<= 1;
                n += bigint_one();
                bits += 1;
                *i += 1;
            }
            Token::Dec => {
                n <<= 1;
                bits += 1;
                *i += 1;
            }
            Token::Out => {
                *i += 1; // Consume terminator
                if bits == 0 {
                    // Extension: input op
                    return Ok(Stmt::InputChar);
                }
                if n.is_zero() {
                    // Extension: pointer left (still uses a number block so it only triggers when bits exist)
                    return Ok(Stmt::MoveLeft);
                }
                if &n == bigint_one() {
                    return Ok(Stmt::MoveRight);
                }
                return Ok(Stmt::Add(n));
            }
            Token::SixSeven => return Err(Error::IllegalTokenInNumberBlock { token_index: *i }),
        }
    }
    Err(Error::UnterminatedNumberBlock { token_index: start })
}

/// Executes a program by running each statement in sequence.
fn exec_program(
    program: &[Stmt],
    m: &mut Machine,
    out: &mut String,
    input: &mut dyn Read,
) -> Result<(), Error> {
    for s in program {
        exec_stmt(s, m, out, input)?;
    }
    Ok(())
}

/// Executes a single statement.
fn exec_stmt(
    s: &Stmt,
    m: &mut Machine,
    out: &mut String,
    input: &mut dyn Read,
) -> Result<(), Error> {
    match s {
        Stmt::Inc => *m.cell_mut() += bigint_one(),
        Stmt::Dec => *m.cell_mut() -= bigint_one(),
        Stmt::Add(n) => *m.cell_mut() += n,
        Stmt::MoveRight => m.ptr += 1,
        Stmt::MoveLeft => m.ptr -= 1,
        Stmt::InputChar => {
            let ch = read_one_char(input)?.unwrap_or('\0');
            *m.cell_mut() = BigInt::from(ch as u32);
        }
        Stmt::Out => {
            let v = match m.tape.get(&m.ptr) {
                Some(v) => v,
                None => {
                    // Cell is zero (not in HashMap), output NUL
                    out.push('\0');
                    return Ok(());
                }
            };
            if v.is_negative() {
                return Err(Error::InvalidCodePoint { value: v.clone() });
            }
            let Some(cp) = v.to_u32() else {
                return Err(Error::InvalidCodePoint { value: v.clone() });
            };
            let Some(ch) = char::from_u32(cp) else {
                return Err(Error::InvalidCodePoint {
                    value: BigInt::from(cp),
                });
            };
            out.push(ch);
        }
        Stmt::While(body) => {
            // Loop while current cell is non-zero (optimized to avoid cloning)
            while !m.cell_is_zero() {
                exec_program(body, m, out, input)?;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn allows_newlines() {
        let toks = tokenize("🫱\n🫲\r\n🤷\n67🫱🤷").unwrap();
        assert_eq!(
            toks,
            vec![
                Token::Inc,
                Token::Dec,
                Token::Out,
                Token::SixSeven,
                Token::Inc,
                Token::Out
            ]
        );
    }

    #[test]
    fn allows_spaces_and_tabs() {
        let toks = tokenize("🫱 \t🫲").unwrap();
        assert_eq!(toks, vec![Token::Inc, Token::Dec]);
    }

    #[test]
    fn ignores_non_instructions_as_comments() {
        let toks = tokenize("🫱x🫲").unwrap();
        assert_eq!(toks, vec![Token::Inc, Token::Dec]);
    }

    #[test]
    fn outputs_a() {
        // 1000001₂ = 65
        let src = "67🫱🫲🫲🫲🫲🫲🫱🤷🤷";
        let out = run_source(src).unwrap();
        assert_eq!(out, "A");
    }

    #[test]
    fn input_empty_number_block_reads_one_char() {
        let src = "67🤷🤷"; // read char -> output it
        let mut input = Cursor::new("Z");
        let out = run_source_with_input(src, &mut input).unwrap();
        assert_eq!(out, "Z");
    }

    #[test]
    fn input_eof_yields_nul() {
        let src = "67🤷🤷";
        let mut input = Cursor::new("");
        let out = run_source_with_input(src, &mut input).unwrap();
        assert_eq!(out, "\0");
    }

    #[test]
    fn move_pointer_right_and_left_via_number_blocks() {
        // `67🫱🤷` => move right
        // `67🫲🤷` => move left
        // build 'A' on cell1, then output cell1 then cell0 (NUL)
        let src = "67🫱🤷67🫱🫲🫲🫲🫲🫲🫱🤷🤷67🫲🤷🤷";
        let out = run_source(src).unwrap();
        assert_eq!(out, "A\0");
    }

    #[test]
    fn control_block_allows_output_via_escape() {
        // cell = 2
        // while cell != 0:
        //   output cell (escaped as 🤷🤷 in block definition)
        //   dec
        // end
        let src = "67🫱🫲🤷6767🤷🤷🫲🤷";
        let out = run_source(src).unwrap();
        assert_eq!(out, "\u{2}\u{1}");
    }
}
