# RMov Reversible Architecture

A program is a set of instructions.

Each memory address must appear once as an input operand
and once as an output operand.

# Instructions

There are four instructions in the RMov instruction set.
All instructions take 2 or 5 memory addresses as operands.

An instruction is inverted by adding or removing `r` in its name.

## `mov A B`
Moves value of `A` to `B`.

```
out[B] <- in[A]
```

## `rmov A B`
Moves value of `B` to `A`.

```
out[A] <- in[B]
```

## `cmov A X B Y Z`
Moves value of `A` to `B`.
If `A` is 1, move from `X` to `Z`,
otherwise, move from `X` to `Y`.

```
out[B] <- in[A]
out[in[A] ? Z : Y] <- in[X]
```

## `rcmov A X B Y Z`
Moves value of `B` to `A`.
If `B` is 1, move from `Y` to `X`,
otherwise, move from `Z` to `X`.

```
out[A] <- in[B]
out[X] <- in[in[B] ? Y : Z]
```

# Pair Multiplexer

Move the value of `x` from `a` to `b` depending on the value of `m`, and
`m` to `ma`. All inputs and outputs are pair-encoded.

```
# inputs: m.0 m.1 x.0 x.1
# outputs: ma.0 ma.1 a.0 a.1 b.0 b.1

# layer 1: t1
mov m.0 p1.0
mov m.1 t1.a
mov x.0 t1.x
mov x.1 p1.1

# layer 2: t2
mov p1.0 p2.0
cmov t1.a t1.x t2.a sa.0 sb.0
mov p1.1 t2.x

# layer 3
cmov t2.a t2.x ma.1 a.1 b.1
mov p2.0 ma.0

```

