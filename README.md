# Chirp

Chirp is a functional, statically typed programming language, created for educational purposes.

It is currently severely lacking features to make it convenient for general use,
so it is most suitable to call it a toy language at this time.

For a list of features, see [features](features.md).

# Installing

Source code can be found in the [src](./src) folder,
and can be built using [CMake](https://cmake.org/).

Requirements:

- CMake 3.19 or newer
- LLVM 12

For example, a simple build on Linux may be produced by:
```bash
mkdir build
cd build
cmake ../src -DCMAKE_BUILD_TYPE=Release
cmake --build .
```

It can then be installed with:
```bash
sudo cmake --install .
```

## Windows

The prebuilt Windows binaries that LLVM distributes do _not_
include an `LLVMConfig.cmake` which is required for
[`find_package`](https://cmake.org/cmake/help/latest/command/find_package.html)
to find it.

Thus, you will have to build LLVM from source yourself, or
[find someone else who has done so for you](https://github.com/vovkos/llvm-package-windows).

# Compiling something

The `bin/chirp2llvm` target emits LLVM IR in plain text on input from stdin.

This can then be compiled with [`llc`](https://llvm.org/docs/CommandGuide/llc.html)
and linked with the runtime (the `chirp_runtime` target) using your system's linker.

For example, on Linux, to compile `prog.crp` (from the build folder),
you can run:

```shell
./bin/chirp2llvm < prog.crp > prog.ll
llc prog.ll
as -ad prog.ll -o prog.o
cc --static -o prog prog.o -L. -lchirp_runtime
```

This will output the `prog` executable.

There is a script [`crpc`](./scripts/crpc) for this,
which is installed with `cmake --install`.

# Example Programs

There are example programs in the [examples](./examples) directory.
Not all of these compile (this is intentional).

# Code Highlighting

There is code highlighting (and auto-indentation) for Emacs [here](./emacs/chirp-mode.el).
This will automatically be loaded by [.dir-locals.el](.dir-locals.el),
or alternatively just open the file and run <kbd>M-x</kbd> `eval-buffer`.
