# RISC-V Symbolic Emulator

![CI](https://github.com/andrewtshen/riscv-symbolic-emulator/workflows/CI/badge.svg)

This project began as a part of the PRIMES program by Andrew Shen under the mentorship of Anish Athalye.
The paper can be found at: https://math.mit.edu/research/highschool/primes/materials/2020/Shen.pdf

## Roadmap
"*A goal without a plan is just a wish*" - Antoine de Saint-Exupery

### emulator
As suggested by the namesake, `emulator` contains the source code for the RISC-V 64-bit symbolic emulator. This section will cover the execution of the emulator from passing a binary file to execution, the supported ISA (and extensions), the peripherals, and the different memory representations. 

**Execution**

The program takes a RISC-V 64-bit binary file as input. After initializing the starting state, the emulator will read from program counter (PC) equals `0x80000000`, which should correspond with the first instruction in the program. Internally, the program counter is saved as `0x0` and then later adjusted to account for the base address using the parameter `base-address`. The emulator will then proceed as specified in base ISA.

The emulator works as follows: loop until program terminates and determine next byte instruction to execute (`emulate.rkt`) --> decode and execute a specific instruction (`execute.rkt`) --> functions for each instruction as specified by the ISA (`instr.rkt`).

**Memory Representation**

To emulate a microcontroller's RAM (or memory) we propose two different representations of RAM:
- **memory as an array**: This is the prefered emulator for running actual code, but fails to quickly reason about large pieces of memory.
- **memory as an uninterpreted function**: This is the prefered representation for verifying the inductive step of our proof as it allows much simpler reasoning for operations across large regions of memory. However, it is not well suited to run large snippets of concrete code as many memory writes result in a large conditional expression, which scales poorly.

### legOS
The simplified kernel that we reason about in our proof. We implemented it in both the ARM and RISC-V variants of Assembly, however for the purposes of our proof, we only reason about the RISC-V variant.

### prerequisite_works
Contains the prerequisite work for this project. There are miscellaneous different projects in here, some complete and some partially complete. Currently, the following directories are included in `prerequisite_works`:
- `lang`, a simple language using Python libraries. Explores syntax trees and how languages are interpreted and compiled.
- `rkt_work`, assorted Racket work, many of which are just small experiements.
- `sign`, a sign function compilied down to both ARM and RISC-V Assembly.
- `software-foundations`, work from the software foundations book.
- `test_write`, example of Rosette, which was used in the presentation as well.
- `z3-proofs`, an assortment of proofs in z3 with Python.

### related_works
Different related works that were used for reference during the making of this project. Currently, the following repositories are included in `related works`:
- `serval-sosp19`
- `serval-tutorial-sosp19`
- `unitary`
- `xv6-public`
- `xv6-riscv-fall19`

### report
The written report of this work and the presentation presented at MIT PRIMES '20.
