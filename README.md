# VecC compiler

An experimental compiler for a vector programming languge. Inspired by ISPC and shader programming.

> [!WARNING]
> This is NOT a finished programming language. Do not use for serious projects.

https://pharr.org/matt/blog/2018/04/30/ispc-all

https://pharr.org/matt/blog/2018/04/21/ispc-volta-c-and-spmd

https://www.cs.cmu.edu/afs/cs/academic/class/15869-f11/www/lectures/07_gpucore.pdf

# Features
- procedural
- strong static type checking
    - type hint system for inferring literal types
- constant folding
- array programming combined with SIMD and AoSoA
- type vectorization
- C codegen backend
- optional semicolons

# Samples

One of the samples is a simple software-rendered game I released on [itch.io](https://jakubtomsu.itch.io/120plus)

> [!NOTE]
> Most samples currently work only on windows

To see the samples in action, first:
```
cd samples
```

then:
```
build <sample_name>
```
or
```
run <sample_name>
```

Samples include:
- `hello_world`
- `game`
- `julia_set`
- `audio`
- `text`

# TODO
- SIMD bytecode?
    - exec mask vs active lane unordered array
- first parameter overloading and self call expressions
- combined vs underlying (native) vector types
- calling scalar procedures with vector parameters
- more vector "intrinsics" as language features, e.g. ISPC's `foreach_active`
- generate multiple versions of procedures based on the parameter vectorization
- polish auto-coversions between array/vector/scalar programming


# Credits
Many parts of this compiler are inspired by [Blaise](https://github.com/gingerBill/blaise) and the [Odin](https://github.com/odin-lang/Odin) compiler.
