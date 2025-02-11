# VecC compiler

An experimental compiler for a vector programming languge. Inspired by ISPC and shader programming.

https://pharr.org/matt/blog/2018/04/30/ispc-all

https://pharr.org/matt/blog/2018/04/21/ispc-volta-c-and-spmd

https://www.cs.cmu.edu/afs/cs/academic/class/15869-f11/www/lectures/07_gpucore.pdf

# Features
- procedural code
- strong static type checking
    - type hint system for inferring literal types
- constant folding
- array programming combined with SIMD and AoSoA
- type vectorization
- C codegen backend


# TODO
- SIMD bytecode?
    - exec mask vs active lane unordered array
- first parameter overloading and self call expressions
- combined vs underlying (native) vector types
