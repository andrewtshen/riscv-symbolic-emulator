# RISC-V Symbolic Emulator

![CI](https://github.com/andrewtshen/riscv-symbolic-emulator/workflows/CI/badge.svg)

This project was built during the PRIMES 2019-2020 program by Andrew Shen under the mentorship of Anish Athalye.
The paper can be found at: https://math.mit.edu/research/highschool/primes/materials/2020/Shen.pdf

## Roadmap to the Directories
For more information on each directory, see the README.md in that sub-directory when one is available.

## Setup and Tests

This repository uses Git submodules for dependency and reference repositories. On Ubuntu, install the expected tools with:

```sh
sudo apt-get update
sudo apt-get install -y racket gcc-riscv64-unknown-elf binutils-riscv64-unknown-elf picolibc-riscv64-unknown-elf qemu-system-misc autoconf make
```

Then initialize the repository and install the Racket package dependencies:

```sh
git submodule update --init --recursive
raco pkg install --auto --batch ./emulator
```

Run the main emulator and RISC-V test suites with:

```sh
raco test emulator/test.rkt
raco test emulator/riscv-tests.rkt
```

The test suites use checked-in binary artifacts under `emulator/build/`. The historical source rebuild target is:

```sh
make -C emulator all
```

On current Ubuntu toolchains, that rebuild path still needs Makefile maintenance for newer RISC-V GCC behavior (`picolibc` specs and the separate `zicsr` ISA extension). The checked-in binary test path above is currently the validated path.

The current repository has no active Git LFS-tracked files (`git lfs ls-files` is empty), although the CI workflow still contains a legacy `git lfs pull` step.

### emulator
Contains the implementation of the RISC-V symbolic emulator, which contains the bulk of the project.

There are two types of RAM memory implementations: 
- array based RAM memory. This is the preferred emulator for running actual code, but fails to quickly reason about large pieces of memory.
- uninterpreted functional based RAM memory. This is the preferred emulator for verifying the inductive step of our proof as it allows much simpler reasoning for operations across large regions of memory. However, it is not as good for running large snippets of concrete code as many memory writes result in a large conditional, which scales poorly.

### legOS
The simplified kernel that we reason about in our proof. We implemented it in both the ARM and RISC-V variants of Assembly, however for the purposes of our proof, we only reason about the RISC-V variant.

### prerequisite_works
Contains the prerequisite work for this project. There are miscellaneous different projects in here, some complete and some partially complete. Currently, the following directories are included in `prerequisite_works`:
- `lang`, a simple language using Python libraries. Explores syntax trees and how languages are interpreted and compiled.
- `rkt_work`, assorted Racket work, many of which are just small experiments.
- `sign`, a sign function compiled down to both ARM and RISC-V Assembly.
- `software-foundations`, work from the software foundations book.
- `test_write`, example of Rosette, which was used in the presentation as well.
- `z3-proofs`, an assortment of proofs in z3 with Python.

### related_works
Contains different related works that were used for reference during the making of this project. Currently, the following repositories are included in `related_works`:
- `serval-sosp19`
- `serval-tutorial-sosp19`
- `unitary`
- `xv6-public`
- `xv6-riscv-fall19`

### report
Contains the written report of this work as well as the presentation presented at MIT PRIMES '20.
