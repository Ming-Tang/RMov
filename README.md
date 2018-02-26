# RMov Reversible Architecture

RMov is a simple architecture for reversible computing. Some features are:

 - Two instructions, move and conditional move
 - Every instruction executes at same time
 - Storage model is an array of bits, only statically addressable

Read `RMov.md` for more details.

Reversible programs can be expressed structurally in terms of blocks.
Each block has inputs, outputs and its body (in terms of sub-blocks).

Read `BD.md` for more details about block-based descriptions of reversible
programs.

# `rmov`

Compile with `g++ --std=gnu++14 rmov.cpp -g -o rmov`

Simulator of RMov in C++.

# `RMovC`

Compile with `ghc RMovC.hs -main-is RMovC -o RMovC`

Converts reversible block defintions into RMov code.

See `Example.bd` for sample input.

 - `RMovC FILE -`: List of block names
 - `RMovC FILE BLOCK instrs`: Display the generated RMov instructions for BLOCK
 - `RMovC FILE BLOCK dot`: Display the Graphviz representation of the generated RMov instructions
 - `RMovC FILE BLOCK cpp`: Display the C++ snippet of the generated RMov instructions

You can run `./runRMovC.sh FILE BLOCK` to display the generated instructions and
its visual representation at same time.

# Sample Output

```
$ ./runRMovC.sh Example.bd mux2
LEmpty {
  LBI: mux2 ((IN:m.0), (IN:m.1), (IN:x.0), (IN:x.1), (IN:y.0), (IN:y.1)) -> ((mux2:a.0#2), (mux2:a.1#2), (mux2:b.0#2), (mux2:b.1#2), (mux2.muxb:a.0#1), (mux2.muxb:a.1), (mux2.muxb:b.0#1), (mux2.muxb:b.1), (mux2.muxb:ma.0), (mux2.muxb:ma.1)) @ mux2 {
    Level: 0 -> 2 {
      LBI: muxa ((IN:m.0), (IN:m.1), (IN:x.0), (IN:x.1)) -> ((mux2.muxa:a.0#1), (mux2.muxa:a.1), (mux2.muxa:b.0#1), (mux2.muxa:b.1), (mux2.muxa:ma.0), (mux2.muxa:ma.1)) @ mux2.muxa {
        Level: 0 -> 1 {
          cmov	(IN:m.1) (IN:x.0) -> (mux2.muxa:r) (mux2.muxa:a.0) (mux2.muxa:b.0)
        }
        Level: 1 -> 2 {
          mov	(IN:x.1) -> (mux2.muxa:x.1#1)
          cmov	(mux2.muxa:r) (mux2.muxa:x.1#1) -> (mux2.muxa:ma.1) (mux2.muxa:a.1) (mux2.muxa:b.1)
          mov	(IN:m.0) -> (mux2.muxa:m.0#1)
          mov	(mux2.muxa:m.0#1) -> (mux2.muxa:ma.0)
        }
        mov	(mux2.muxa:a.0) -> (mux2.muxa:a.0#1)
        mov	(mux2.muxa:b.0) -> (mux2.muxa:b.0#1)
      }
    }
    Level: 2 -> 4 {
      mov	(IN:y.0) -> (mux2:y.0#1)
      mov	(mux2:y.0#1) -> (mux2:y.0#2)
      mov	(IN:y.1) -> (mux2:y.1#1)
      mov	(mux2:y.1#1) -> (mux2:y.1#2)
      LBI: muxb ((mux2.muxa:ma.0), (mux2.muxa:ma.1), (mux2:y.0#2), (mux2:y.1#2)) -> ((mux2.muxb:a.0#1), (mux2.muxb:a.1), (mux2.muxb:b.0#1), (mux2.muxb:b.1), (mux2.muxb:ma.0), (mux2.muxb:ma.1)) @ mux2.muxb {
        Level: 2 -> 3 {
          cmov	(mux2.muxa:ma.1) (mux2:y.0#2) -> (mux2.muxb:r) (mux2.muxb:a.0) (mux2.muxb:b.0)
        }
        Level: 3 -> 4 {
          mov	(mux2:y.1#2) -> (mux2.muxb:x.1#1)
          cmov	(mux2.muxb:r) (mux2.muxb:x.1#1) -> (mux2.muxb:ma.1) (mux2.muxb:a.1) (mux2.muxb:b.1)
          mov	(mux2.muxa:ma.0) -> (mux2.muxb:m.0#1)
          mov	(mux2.muxb:m.0#1) -> (mux2.muxb:ma.0)
        }
        mov	(mux2.muxb:a.0) -> (mux2.muxb:a.0#1)
        mov	(mux2.muxb:b.0) -> (mux2.muxb:b.0#1)
      }
    }
    mov	(mux2.muxa:a.0#1) -> (mux2:a.0#1)
    mov	(mux2:a.0#1) -> (mux2:a.0#2)
    mov	(mux2.muxa:a.1) -> (mux2:a.1#1)
    mov	(mux2:a.1#1) -> (mux2:a.1#2)
    mov	(mux2.muxa:b.0#1) -> (mux2:b.0#1)
    mov	(mux2:b.0#1) -> (mux2:b.0#2)
    mov	(mux2.muxa:b.1) -> (mux2:b.1#1)
    mov	(mux2:b.1#1) -> (mux2:b.1#2)
  }
}

```

[Diagram for `mux2`](https://gist.github.com/Ming-Tang/58607a987c4439555f33490caf09ef43)

