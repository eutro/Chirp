# Building

Source code can be found in the [src](./src) folder,
and can be built using [CMake](https://cmake.org/).

For example, a simple build may be produced by:
```bash
mkdir build
cd build
cmake ../src
cmake --build .
```

Final executable binaries will be output in the `bin` folder in your build directory,
corresponding to the targets in the [bin](./src/bin) source folder.

# Compiling something

The `bin/in2llvm` target emits LLVM IR in plain text on input from stdin.

This can then be compiled with [`llc`](https://llvm.org/docs/CommandGuide/llc.html)
and linked with the runtime (the `crp` target) using your system's linker.

For example, on Linux, to compile `prog.crp` (from the build folder),
you can run:

```shell
./bin/in2llvm < prog.crp > prog.ll
llc prog.ll
as -ad prog.ll -o prog.o
cc --static -o prog prog.o -L. -lcrp
```

This will output the `prog` executable.

There is a script [`crpc`](./scripts/crpc) for this.

# Example Programs

There are example programs in the [examples](./examples) directory.
Not all of these compile (this is intentional).

# Code Highlighting

There is text highlighting for Emacs [here](./emacs/chirp-mode.el).
Just open the file and run <kbd>M-x</kbd> `eval-buffer`.
