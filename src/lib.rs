use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::HashMap;
use std::io::Read;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Inc,      // 🫱
    Dec,      // 🫲
    Out,      // 🤷
    SixSeven, // 67
}

#[derive(Debug)]
pub enum Error {
    IllegalChar { byte_index: usize, ch: char },
    DanglingSix { byte_index: usize },
    UnterminatedNumberBlock { token_index: usize },
    IllegalTokenInNumberBlock { token_index: usize },
    UnterminatedControlBlock { token_index: usize },
    UnexpectedToken { token_index: usize },
    InvalidCodePoint { value: BigInt },
    Io(std::io::Error),
    InvalidUtf8Input,
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, Error> {
    let mut out = Vec::new();
    let mut it = input.char_indices().peekable();

    while let Some((i, ch)) = it.next() {
        match ch {
            // Allowed whitespace: newlines (Windows / Unix) + spaces + tabs
            '\n' | '\r' | ' ' | '\t' => {}
            '🫱' => out.push(Token::Inc),
            '🫲' => out.push(Token::Dec),
            '🤷' => out.push(Token::Out),
            '6' => match it.peek() {
                Some((_, '7')) => {
                    it.next();
                    out.push(Token::SixSeven);
                }
                Some((_, _)) => return Err(Error::IllegalChar { byte_index: i, ch }),
                None => return Err(Error::DanglingSix { byte_index: i }),
            },
            _ => return Err(Error::IllegalChar { byte_index: i, ch }),
        }
    }

    Ok(out)
}

#[derive(Debug, Default)]
struct Machine {
    ptr: i64,
    tape: HashMap<i64, BigInt>,
}

impl Machine {
    fn cell_mut(&mut self) -> &mut BigInt {
        self.tape.entry(self.ptr).or_insert_with(BigInt::zero)
    }

    fn cell(&self) -> BigInt {
        self.tape.get(&self.ptr).cloned().unwrap_or_else(BigInt::zero)
    }
}

pub fn run_source(source: &str) -> Result<String, Error> {
    let tokens = tokenize(source)?;
    run_tokens(&tokens)
}

pub fn run_tokens(tokens: &[Token]) -> Result<String, Error> {
    let mut input = std::io::empty();
    run_tokens_with_input(tokens, &mut input)
}

pub fn run_source_with_input(source: &str, input: &mut dyn Read) -> Result<String, Error> {
    let tokens = tokenize(source)?;
    run_tokens_with_input(&tokens, input)
}

pub fn run_tokens_with_input(tokens: &[Token], input: &mut dyn Read) -> Result<String, Error> {
    let program = parse_program_m0(tokens)?;
    let mut m = Machine::default();
    let mut out = String::new();
    exec_program(&program, &mut m, &mut out, input)?;
    Ok(out)
}

fn read_one_char(input: &mut dyn Read) -> Result<Option<char>, Error> {
    let mut buf = Vec::<u8>::new();
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

#[derive(Debug, Clone)]
enum Stmt {
    Inc,
    Dec,
    Out,
    InputChar, // `67🤷` (empty bit_seq)
    MoveRight, // `67🫱🤷` (N==1)
    MoveLeft,  // `67🫲🤷` (N==0, with at least one bit)
    Add(BigInt),
    While(Vec<Stmt>),
}

fn parse_program_m0(tokens: &[Token]) -> Result<Vec<Stmt>, Error> {
    let mut i = 0usize;
    let mut out = Vec::new();
    while i < tokens.len() {
        out.push(parse_stmt_m0(tokens, &mut i)?);
    }
    Ok(out)
}

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
                // control block: 67 67 ... 🤷  (in M2: single 🤷 closes, 🤷🤷 encodes literal output)
                *i += 2;
                let body = parse_block_m2(tokens, i)?;
                Ok(Stmt::While(body))
            } else {
                // number block / input / pointer move
                parse_number_like(tokens, i)
            }
        }
    }
}

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

fn parse_block_m2(tokens: &[Token], i: &mut usize) -> Result<Vec<Stmt>, Error> {
    let mut body = Vec::new();
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

fn parse_number_like(tokens: &[Token], i: &mut usize) -> Result<Stmt, Error> {
    // expects current token is 67
    let start = *i;
    *i += 1; // consume 67
    let mut n = BigInt::zero();
    let mut bits = 0usize;
    while *i < tokens.len() {
        match tokens[*i] {
            Token::Inc => {
                n <<= 1;
                n += BigInt::one();
                bits += 1;
                *i += 1;
            }
            Token::Dec => {
                n <<= 1;
                bits += 1;
                *i += 1;
            }
            Token::Out => {
                *i += 1; // consume terminator
                if bits == 0 {
                    // extension: input op
                    return Ok(Stmt::InputChar);
                }
                if n.is_zero() {
                    // extension: pointer left (still uses a number block so it only triggers when bits exist)
                    return Ok(Stmt::MoveLeft);
                }
                if n == BigInt::one() {
                    return Ok(Stmt::MoveRight);
                }
                return Ok(Stmt::Add(n));
            }
            Token::SixSeven => return Err(Error::IllegalTokenInNumberBlock { token_index: *i }),
        }
    }
    Err(Error::UnterminatedNumberBlock { token_index: start })
}

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

fn exec_stmt(s: &Stmt, m: &mut Machine, out: &mut String, input: &mut dyn Read) -> Result<(), Error> {
    match s {
        Stmt::Inc => *m.cell_mut() += BigInt::one(),
        Stmt::Dec => *m.cell_mut() -= BigInt::one(),
        Stmt::Add(n) => *m.cell_mut() += n,
        Stmt::MoveRight => m.ptr += 1,
        Stmt::MoveLeft => m.ptr -= 1,
        Stmt::InputChar => {
            let ch = read_one_char(input)?.unwrap_or('\0');
            *m.cell_mut() = BigInt::from(ch as u32);
        }
        Stmt::Out => {
            let v = m.cell();
            if v.is_negative() {
                return Err(Error::InvalidCodePoint { value: v });
            }
            let Some(cp) = v.to_u32() else {
                return Err(Error::InvalidCodePoint { value: v });
            };
            let Some(ch) = char::from_u32(cp) else {
                return Err(Error::InvalidCodePoint { value: BigInt::from(cp) });
            };
            out.push(ch);
        }
        Stmt::While(body) => {
            while !m.cell().is_zero() {
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
    fn rejects_other_chars() {
        let err = tokenize("🫱x🫲").unwrap_err();
        matches!(err, Error::IllegalChar { .. });
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


