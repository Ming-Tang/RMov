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

