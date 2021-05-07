# RISC-V Symbolic Emulator

![CI](https://github.com/andrewtshen/riscv-symbolic-emulator/workflows/CI/badge.svg)

This project began as a part of the [PRIMES](https://math.mit.edu/research/highschool/primes/) program by Andrew Shen under the mentorship of Anish Athalye.
The paper, albiet slightly outdated, can be found [here](https://math.mit.edu/research/highschool/primes/materials/2020/Shen.pdf).

## Roadmap
"*A goal without a plan is just a wish*" - Antoine de Saint-Exupery
- [emulator](#emulator)
- [legOS](#legOS)
- [prerequisite_works](#prerequisite_works)
- [related_works](#related_works)
- [report](#report)


<a name="emulator"/>

## emulator
As suggested by the name, `emulator` contains the source code for the RISC-V 64-bit symbolic emulator. This section will cover the execution of the emulator from passing a binary file to execution, the supported instruction set (and extensions), the peripherals, and the different memory representations. 

### Execution
The program takes a RISC-V 64-bit binary file as input. After initializing the starting state, the emulator will read from program counter (PC) equals `0x80000000`, which should correspond with the first instruction in the program. Internally, the program counter is saved as `0x0` and then later adjusted to account for the base address using the parameter `base-address`. The emulator will then proceed as specified in base ISA.

The emulator works as follows: `emulate.rkt` will loop until the program terminates by reading either word (4 byte) or half-word (2 byte) instructions from the program. Then, `execute.rkt` determines next byte instruction to execute by decoding it and calls a function from `instr.rkt` which makes changes to the machine states as specified by the [ISA](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf). Execution continues until the program reads an `mret` instruction or encounters an `illegal-instruction` error. 

*TODO: At this time `mret` is a de-facto stop instruction, which happened to be convient for the programs we tested. A more faithful implementation will be updated later on.*

### Supported Instruction Set and Extensions
The emulator currently supports the following base instruction set and extensions as well as performs the corresponding checks from [riscv-tests](https://github.com/riscv/riscv-tests).
| Name  | Description                                                 | riscv-tests |
|-------|-------------------------------------------------------------|-------------|
| RV64I | Base Integer Instruction Set                                | rv64ui      |
| "M"   | Standard Extension for Integer Multiplication and Division  | rv64um      |
| "C"   | Standard Extension for Compressed Instructions              | rv64uc      |

### Peripherals
There are currently no peripherals implemented. However, there are plans to implement UART in the near future.

### Memory Representation
To emulate a microcontroller's RAM (or memory) we propose two different representations of RAM:
- **memory as an array**: This is the prefered emulator for running actual code, but fails to quickly reason about large pieces of memory.
- **memory as an uninterpreted function**: This is the prefered representation for verifying the inductive step of our proof as it allows much simpler reasoning for operations across large regions of memory. However, it is not well suited to run large snippets of concrete code as many memory writes result in a large conditional expression, which scales poorly.


<a name="legOS"/>

## legOS
A simplified microcontroller kernel, which is implemented in both ARM and RISC-V Assembly. In our proof, we only reason about the RISC-V variant.


<a name="prerequisite_works"/>

## prerequisite_works
Contains the prerequisite work for this project. There are miscellaneous different projects in here, some complete and some partially complete. Currently, the following directories are included in `prerequisite_works`:
- `lang`, a simple language using Python libraries. Explores syntax trees and how languages are interpreted and compiled.
- `rkt_work`, assorted Racket work, many of which are just small experiements.
- `sign`, a sign function compilied down to both ARM and RISC-V Assembly.
- `software-foundations`, work from the software foundations book.
- `test_write`, example of Rosette, which was used in the presentation as well.
- `z3-proofs`, an assortment of proofs in z3 with Python.


<a name="related_works"/>

## related_works
Different related works that were used for reference during the making of this project. Currently, the following repositories are included in `related works`:
- `serval-sosp19`
- `serval-tutorial-sosp19`
- `unitary`
- `xv6-public`
- `xv6-riscv-fall19`



<a name="report"/>

## report
The written report of this work and the presentation presented at MIT PRIMES '20.
