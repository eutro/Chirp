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
