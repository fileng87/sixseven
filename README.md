# sixseven

A tiny Rust interpreter for the **sixseven** language, born from the **“67” meme**.

The language has only four instructions: `67`, `🫲`, `🫱`, `🤷`.

Any other character is treated as a **comment** and ignored. (`6` not followed by `7` is ignored too.)

## Execution model

- Infinite tape of integer cells (`BigInt`, **no overflow**)
- One data pointer (starts at cell 0); all cells start at 0

## Core semantics (M0 default)

- `🫱`: current cell += 1
- `🫲`: current cell -= 1
- `🤷`: output current cell as a **Unicode code point**
- `67`: starts a *number-like* construct (number / input / pointer move) or a control block

## Number blocks (M1: integer parsing)

Syntax: `67` bit_seq `🤷`

- `🫱` = bit `1`, `🫲` = bit `0` → parse binary integer \(N\)
- End-of-block behavior (**extensions** to avoid adding tokens):
  - `67🤷` (empty bit_seq): read **1 Unicode scalar** from input and **overwrite** current cell with its code point (EOF → `0`)
  - `67🫲🤷` (bit_seq non-empty and \(N=0\)): move pointer **left** by 1
  - `67🫱🤷` (bit_seq non-empty and \(N=1\)): move pointer **right** by 1
  - otherwise: current cell += \(N\)

## Control blocks (M2: while block)

Syntax: `67 67` block `🤷`

Semantics: `while (current cell ≠ 0) { execute block }`

Inside a block definition:
- `🤷` ends the **current** block
- `🤷🤷` is a literal output instruction inside the block body (also enables nesting)

## CLI

### REPL Mode

Enter interactive REPL mode:

```bash
cargo run
# or
sixseven
```

In REPL mode:
- Enter code line by line and execute immediately
- Type `exit` or `quit` to exit
- Press Ctrl+D (Unix) or Ctrl+Z (Windows) to exit

### Execute Code from Command Line

Execute code directly (like `node -e`):

```bash
cargo run -- -e "67🫱🫲🫲🫲🫲🫲🫱🤷🤷"
# or
sixseven -e "67🫱🫲🫲🫲🫲🫲🫱🤷🤷"
```

### Execute File

- Program from file, input from stdin (default):

```bash
cargo run -- program.67
# or
sixseven program.67
```

- Program from file, input from file:

```bash
cargo run -- program.67 input.txt
# or
sixseven program.67 input.txt
```

## Examples

- `example/hello_world.67`: prints `Hello, World!`
- `example/hello_input.67`: prints `Hello, `, then reads and echoes input until newline or EOF, then prints `!` (e.g., input `World` produces `Hello, World!`)
- `example/echo.67`: reads 1 char and prints it (source is `67🤷🤷`)
