PREFIX ?= riscv64-unknown-elf-
DEBUG ?= 1

QEMU := qemu-system-riscv64
QEMUFLAGS := -M virt -nographic -bios none

CC := $(PREFIX)gcc
GDB := $(PREFIX)gdb
OBJDUMP := $(PREFIX)objdump
OBJCOPY := $(PREFIX)objcopy
# OBJDUMPFLAGS := --disassemble-all --source --section-headers --demangle
OBJDUMPFLAGS := --source --demangle

CFLAGS := -ffreestanding -nostdlib -O0 -mcmodel=medany -march=rv64i
ifeq ($(DEBUG),1)
	CFLAGS += -g
endif

SUM_OBJS := \
	sum.o \

STORE_AND_LOAD_OBJS := \
	store_and_load.o \

CSR_TEST_OBJS := \
	csr_test.o

BUILDS := \
	sum.bin sum.lst \
	store_and_load.bin store_and_load.lst \
	csr_test.bin csr_test.lst \

.PHONY: all
all: $(BUILDS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $< -o $@

sum.elf: $(SUM_OBJS) prog.ld
	$(CC) $(CFLAGS) -T prog.ld $(SUM_OBJS) -o $@

store_and_load.elf: $(STORE_AND_LOAD_OBJS) prog.ld
	$(CC) $(CFLAGS) -T prog.ld $(STORE_AND_LOAD_OBJS) -o $@

csr_test.elf: $(CSR_TEST_OBJS) prog.ld
	$(CC) $(CFLAGS) -T prog.ld $(CSR_TEST_OBJS) -o $@

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.lst: %.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@

.PHONY: emulate
emulate: sum.bin store_and_load.bin
	racket emulate.rkt

.PHONY: proof
proof: sum.bin store_and_load.bin
	raco test test.rkt

.PHONY: test
test: sum.bin store_and_load.bin
	@echo "----------------------"
	@echo "> starting load.rkt"
	racket load.rkt
	@echo "> ending load.rkt"
	@echo "----------------------"
	@echo "> starting decode.rkt"
	racket decode.rkt
	@echo "> ending decode.rkt"
	@echo "----------------------"
	@echo "> starting instr.rkt"
	racket instr.rkt
	@echo "> ending instr.rkt"
	@echo "----------------------"
	@echo "> starting execute.rkt"
	racket execute.rkt
	@echo "> ending execute.rkt"
	@echo "----------------------"
	@echo "> starting emulate.rkt"
	racket emulate.rkt
	@echo "> ending emulate.rkt"
	@echo "----------------------"

.PHONY: clean
clean:
	rm -f *.o *.lst *.elf *.bin

.PHONY: qemu
qemu: sum.elf
	$(QEMU) $(QEMUFLAGS) -kernel sum.elf

.PHONY: qemu-gdb
qemu-gdb: sum.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel sum.elf

.PHONY: gdb
gdb: sum.elf
	$(GDB) --data-directory=/usr/local/share/gdb sum.elf
