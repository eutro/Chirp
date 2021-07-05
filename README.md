# Building

Source code can be found in the [src](./src) folder, and can be built using [CMake](https://cmake.org/).

For example, a simple build may be produced by:
```bash
mkdir build
cd build
cmake ../src
cmake --build .
```

Executable binaries will be output in the `bin` folder in your build directory,
corresponding to the targets in the [bin](./src/bin) source folder.

# Compiling something

The `in2llvm` executable emits LLVM IR in plain text on input from stdin.

This can then be compiled with `llc` and linked with the runtime.

For example (from the build folder):

```shell
./bin/in2llvm < prog.crp > prog.ll
llc --filetype=obj prog.ll
gcc --static -o prog prog.o -L. -lcrp
```
