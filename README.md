# primes2019

## Roadmap to the Directories
For more information on each of the directories use the README.md in each of the directories.

### emulate
`emulate` contains the symbolic RISC-V machine emulator with array based RAM memory. This is the prefered emulator for running actual code, but fails to quickly reason about large pieces of memory.

### emulate_fnmem
`emulate_fnmem` contains the symboic RISCV-V machine emulator with uninterpreted functional based RAM memory. This is the prefered emulator for verifying the inductive step of our proof as it allows much simpler reasoning for operations across large regions of memory. However, it is not as good for running large snippets of concrete code as many memory writes result in a large conditional, which scales poorly.

### legOS
`legOS` is the simplified kernel that we reason about in our proof. We implemented it in both the ARM and RISC-V variants of Assembly, however for the purposes of our proof, we only reason about the RISC-V variant as it is a little simpler.

### prerequisite_works
`prerequisite_works` contains the prerequisite work for this project. There is miscellaneous different projects in here, some complete and some partially complete.

### related_works
`related_works` contains different related works that were used for reference during the making of this project.

### presentation
`presentation` is the presentation of this work in the format of slides and speaker notes.
