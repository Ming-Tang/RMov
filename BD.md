# Block Definitions

A block is a reversible program with input and output variables,
and to match the RMov memory model, all variables are bits (0 or 1).

Intermediate variables are variables that occur in the block body but
are not used for inputs or outputs.

See `Example.bd` for a some sample block definitions.

The syntax for a block is `block-name { parts }`, where a part is
`in { var-list }` for inputs, `out { var-list }` for outputs,
`int { var-list }` for intermediate variables and `body { ... }`
for block body. The `int` section is optional.

The block body defines the contents of a block in terms of subblocks.

The block body is composed of multiple levels, and each level is composed
of one or more statements.

A statement is either move statement (`output-vars <- input-vars`) or
subblock statement `output-vars = sb(input-vars)`, where
`output-vars` and `input-vars` are variable lists.
A subblock name (`sb`) is one of:

 - `T`: Conditional move `mov`
 - `~T`: Reverse conditional move `rmov`
 - `bi:b`: Apply subblock `b` with unique instance name `bi`

Statements are separated by semicolon `;` and levels are separated
using the divider, which is one or more `-`.

# Variable Lists

A variable list of a comma-separated list of variables with two
additional syntaxes.

The `var*` syntax expands to two variables `var.0, var.1`.

If multiple consecutive variables `vars` share a suffix
(`.0`, `.1` or `*`), they can be expressed using the `[vars] suffix`
syntax. Examples:

 - `[a, b].0` becomes `a.0, b.0`
 - `[a, b, c].1` becomes `a.1, b.1, c.1`
 - `[a, b, c]*` becomes `a.0, a.1, b.0, b.1, c.0, c.1`

The `*` syntax exists because many variables are pair-encoded.

