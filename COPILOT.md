# Current project state

Generated from local repository inspection on 2026-07-26.

## Repository status

- Repository: `andrewtshen/riscv-symbolic-emulator`
- Branch: `master`
- Git state before this file was added: clean and tracking `origin/master`
- Latest commit inspected: `c7b0209 Merge pull request #6 from andrewtshen/test`
- Configured submodules are initialized locally, including `emulator/riscv-tests`, `emulator/riscv-tests/env`, `legOS`, `prerequisite_works/software-foundations`, `prerequisite_works/z3-proofs`, and the `related_works/*` repositories.

## Project summary

This is a Racket/Rosette RISC-V symbolic emulator project from PRIMES 2019-2020. The stated goal is to reason about a simplified hardware-wallet-style kernel and prove application isolation properties, especially around PMP-protected memory.

The active implementation is under `emulator/`. Other top-level areas are mostly supporting material:

- `emulator/`: Racket/Rosette emulator, tests, RISC-V test programs, and a small RISC-V C kernel.
- `prerequisite_works/`: older experiments in Python, Racket, ARM/RISC-V assembly, Rosette, and related proof work.
- `report/`: paper, presentation, diagrams, and bibliography.
- `.github/workflows/ci.yml`: CI workflow for Racket/Rosette tests.

## Local tooling and validation state

Required local tools are now installed:

- `racket`: present, Racket v8.10 [cs]
- `raco`: present
- `riscv64-unknown-elf-gcc`: present, GCC 13.2.0
- `riscv64-unknown-elf-objcopy`: present, GNU objcopy 2.42
- `picolibc-riscv64-unknown-elf`: present, version 1.8.6-2. Ubuntu installs headers under `/usr/lib/picolibc/riscv64-unknown-elf`; `riscv64-unknown-elf-gcc` finds them when invoked with `--specs=picolibc.specs`.
- `qemu-system-riscv64`: present, QEMU 8.2.2
- `git-lfs`: present, git-lfs 3.4.1
- `autoconf`: present, GNU Autoconf 2.71
- `make`: present, GNU Make 4.3

Racket package setup has been run:

```sh
git submodule update --init --recursive
raco pkg install --auto --batch ./emulator
```

The local `emulator` package is linked, and `rosette` plus its dependencies are installed for Racket 8.10.

There are existing build artifacts under `emulator/build/`: 418 files total, including 403 `riscv-tests` `.bin` files and 15 local test `.bin` files. The checked-in binary test path is validated.

`raco make emulator/*.rkt` passes. `make -C emulator kernel/kernel.bin kernel/user.bin` reports the kernel/user binaries are up to date. A full `make -C emulator all` no longer fails on missing `string.h` after installing `picolibc`, but the historical rebuild path still needs Makefile maintenance for current toolchains:

- Ubuntu's `riscv64-unknown-elf-gcc` needs `--specs=picolibc.specs` to find `picolibc` headers.
- `emulator/riscv-tests.mk` can recurse into a bare `make` when no upstream `.dump` files exist yet.
- Local C builds use `-march=rv64i`, but GCC/binutils 13 require the CSR extension to be explicit for CSR instructions, e.g. `-march=rv64i_zicsr`.

CI is configured to install Racket 8.0, pull Git LFS files, install the local emulator package, and run:

```sh
raco pkg install --auto --batch ./emulator
raco test emulator/test.rkt
raco test emulator/riscv-tests.rkt
```

The CI-equivalent test commands have been run locally with the current setup:

- `raco test emulator/test.rkt`: passed, 27 tests.
- `raco test emulator/riscv-tests.rkt`: passed, 51 tests.

## Emulator architecture

Important emulator modules:

- `emulator/init.rkt`: machine initialization, program loading, bytearray conversion, default CSR/PMP setup.
- `emulator/machine.rkt`: CPU and machine structs, GPR/CSR helpers, memory reads/writes, vector memory, uninterpreted-function memory, and PMP-guarded RAM access.
- `emulator/fmt.rkt`: opcode-to-format classification.
- `emulator/execute.rkt`: decodes 32-bit instructions directly from bit fields and dispatches to instruction semantics.
- `emulator/instr.rkt`: instruction semantics and PC/register/memory updates.
- `emulator/pmp.rkt`: PMP config/address structs, NAPOT decoding, and PMP access checks.
- `emulator/csrs.rkt`: CSR constants and CSR storage helpers.
- `emulator/emulate.rkt`: `step`, `execute-until-mret`, and `execute-until-ecall`.
- `emulator/parameters.rkt`: runtime parameters such as symbolic optimizations, memory representation, RAM size, and base address.
- `emulator/concrete-optimizations.rkt`: concrete fast paths for PMP and bytearray reads.
- `emulator/test.rkt`: main rackunit/Rosette test and proof suite.
- `emulator/riscv-tests.rkt`: RV64UI-style binary test runner.

The root README and emulator README have been updated to use the current `emulator/` directory name and the current `execute.rkt` decode/dispatch flow.

## Current functionality

The implemented path can initialize a symbolic or concrete machine, load binary programs, fetch 32-bit instructions, decode them, execute many RV64I operations, model PMP-protected memory access, and run proof-oriented tests around boot and isolation properties.

Default parameters include:

- `use-sym-optimizations`: `#f`
- `use-fnmem`: `#t`
- `use-concrete-mem`: `#f`
- `use-concrete-optimizations`: `#f`
- `ramsize-log2`: `20`
- `base-address`: `#x80000000`

The simplified kernel in `emulator/kernel/` sets PMP regions, sets `mstatus`, hardcodes `mtvec` to `0x80000080`, loads `user.bin` to `0x80020000`, and enters user mode with `mret`.

## Tests and proofs present

`emulator/test.rkt` contains:

- Individual instruction sanity tests for local `build/*.bin` programs.
- High-level stack, PMP, and kernel boot tests.
- PMP utility and instruction decoding tests.
- Single-step symbolic properties for memory isolation, mode behavior, trap-vector reachability, and non-null step results.
- Boot sequence and inductive-step proof checks for the OK/isolation state.

`emulator/riscv-tests.rkt` contains 51 RV64UI-style tests over `build/riscv-tests/rv64ui-p-*.bin`, including arithmetic, branches, jumps, loads/stores, shifts, comparisons, `fence_i`, and simple tests.

## Known incomplete or fragile areas

- `machine-ram-read` computes read end addresses as `addr + nbytes * 8` instead of the last byte address, so PMP read checks are too strict near region boundaries.
- Illegal memory reads return `'illegal-instruction`, but load instructions immediately pass that symbol to `sign-extend` or `zero-extend`, causing a runtime contract error instead of clean trap/illegal-instruction behavior.
- `slliw`, `srliw`, and `sraiw` write `rd` before checking the reserved `imm[5]` bit, so reserved encodings can mutate machine state before returning `'illegal-instruction`.
- `execute-R` omits opcode checks on some R-type cases (`slt`, `sltu`, `xor`, `or`, `and`), so invalid OP-32 encodings can execute as 64-bit OP instructions.
- `andi-instr` returns a decoded label of `'addi` instead of `'andi`; `lb`, `lh`, `lw`, and `ld` return `'m` instead of their instruction names.
- Illegal instruction/trap handling is incomplete: `illegal-instr` exists but `step`/`execute` mostly return `'illegal-instruction` without consistently updating `pc`/mode to trap state.
- Several instruction semantics are stubs or placeholders: `ebreak`, `uret`, `dret`, `sfence_vma`, `wfi`, `csrrc`, `csrrsi`, `csrrci`, the RV64M multiply/divide/remainder family, and `andw`.
- `ecall` is only a marker, not a real trap implementation.
- `FENCE` and `FENCE_I` are implemented as PC-advancing no-ops.
- CSR permission behavior is incomplete.
- `mret` forces user mode instead of fully restoring mode from `mstatus.MPP`.
- PMP handling only supports the currently needed NAPOT path and has TODOs for access type, locking behavior, and other modes.
- `init.rkt` has TODOs around realistic initial CSR/PMP values.
- `emulator/kernel/kernel.c` has hardcoded or TODO-marked values for user memory sizing, UART/PMP behavior, `mtvec`, and copy size.
- There are no active Git LFS-tracked files (`git lfs ls-files` is empty), although CI still runs `git lfs pull`.

## Useful next steps

1. Modernize the Makefile rebuild path: pass `--specs=picolibc.specs` for upstream `riscv-tests`, avoid the empty-target recursion in `all-riscv-tests`, and update local `CFLAGS` to include `zicsr`.
2. Add regression tests for the concrete semantic bugs listed above, then fix them in small patches.
3. Decide whether CI should keep using checked-in binaries or rebuild them from source; if it rebuilds, add the RISC-V toolchain and C library to CI.
4. Remove or justify the no-op `git lfs pull` step if there are no LFS-tracked files.
5. Triage the remaining instruction/PMP/kernel TODOs before extending proof coverage.
