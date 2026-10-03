# RISC-V Symbolic Emulator

### Method
The main execution helpers are in `emulate.rkt`. Machine state and program loading are handled by `init.rkt` and `machine.rkt`. Instructions are read in 4-byte chunks, classified by `fmt.rkt`, decoded directly in `execute.rkt`, and then dispatched to the instruction semantics in `instr.rkt`.

`emulate.rkt` -> `init.rkt` / `machine.rkt` -> `fmt.rkt` -> `execute.rkt` -> `instr.rkt`

### Setup and Tests

From the repository root:

```sh
git submodule update --init --recursive
raco pkg install --auto --batch ./emulator
raco test emulator/test.rkt
raco test emulator/riscv-tests.rkt
```

The checked-in binaries under `build/` are enough for the test commands above. The historical rebuild target is:

```sh
make -C emulator all
```

On current Ubuntu toolchains, this path still needs Makefile maintenance for newer RISC-V GCC behavior (`picolibc` specs and the separate `zicsr` ISA extension).

### Files
##### `init.rkt`
Initialize the machine and load the program into the machine to run symbolically. 

##### `machine.rkt`
Contains the necessary registers and structs defining those registers for the machine. Initialize mutator and accessor functions for changing the memory in the function.

##### `fmt.rkt`
Return the instruction format for each of the opcodes.

##### `execute.rkt`
Decode binary instructions and dispatch to the instruction implementations.

##### `instr.rkt`
Execute each individual instruction symbolically and update the program counter, registers, and memory as needed.

##### `pmp.rkt`
Model physical memory protection configuration, NAPOT decoding, and access checks.

##### `csrs.rkt`
Define CSR constants and CSR storage helpers.

##### `parameters.rkt`
Define runtime parameters for memory representation, symbolic optimizations, debug output, RAM size, and base address.

##### `emulate.rkt`
Set up the machine and execute each instruction. Properties are proved at the end of the symbolic execution.

##### `test.rkt`
Main rackunit/Rosette test suite for instruction sanity checks, PMP utilities, boot behavior, and proof-oriented isolation properties.

##### `riscv-tests.rkt`
Runner for the RV64UI-style binary tests under `build/riscv-tests`.
