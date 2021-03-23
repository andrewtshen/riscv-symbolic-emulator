# RISC-V Symbolic Emulator

### TODO: FIX FILES AFTER REFORMATING AND NUMEROUS CHANGES TO STRUCTURE

### Method
The main program is `emulate.rkt` which sets up the machine from and loads the program to be run from `init.rkt`. Then instructions are read in 4-bytes, converted to their readable forms using `fmt.rkt` to get the format, `decode.rkt` to convert the binary to the normal format. `execute.rkt` finally performs that decoded symbolic value.

`emulate.rkt` -> `init.rkt` -> `fmt.rkt` -> `decode.rkt` -> `execute.rkt`  

### Files
##### `init.rkt`
Initialize the machine and load the program into the machine to run symbolically. 

##### `machine.rkt`
Contains the necessary registers and structs defining those registers for the machine. Initialize mutator and accessor functions for changing the memory in the function.

##### `decode.rkt`
Decode all of the binary instructions to a list similar to the objdump output so that it is easier to parse.

##### `fmt.rkt`
Return the instruction format for each of the opcodes.

##### `execute.rkt`
Execute each individual instruction symbolically and update the program count to the proper place. Used rv8.io for implementing instructions

##### `emulate.rkt`
Set up the machine and execute each instruction. Properties are proved at the end of the symbolic execution.
