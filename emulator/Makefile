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

CFLAGS := -ffreestanding -nostdlib -Os -mcmodel=medany -march=rv64i -mabi=lp64
ifeq ($(DEBUG),1)
	CFLAGS += -g
endif

BUILD = build
CBUILD = build/cbuild
SBUILD = build/sbuild
TESTS = tests

C_STARTUP = startup.s

V = 0
ACTUAL_CC := $(CC)

CC_0 = @printf ' |%-10s| $@ \n' 'CC'; $(ACTUAL_CC)
CC_1 = $(ACTUAL_CC)
CC = $(CC_$(V))

ACTUAL_OBJDUMP := $(OBJDUMP)
OBJDUMP_0 = @printf ' |%-10s| $@ \n' 'OBJDUMP'; $(ACTUAL_OBJDUMP)
OBJDUMP_1 = $(ACTUAL_OBJDUMP)
OBJDUMP = $(OBJDUMP_$(V))

ACTUAL_OBJCOPY := $(OBJCOPY)
OBJCOPY_0 = @printf ' |%-10s| $@ \n' 'OBJCOPY'; $(ACTUAL_OBJCOPY)
OBJCOPY_1 = $(ACTUAL_OBJCOPY)
OBJCOPY = $(OBJCOPY_$(V))

BUILDS := \
	$(patsubst tests/%.c,%.bin,$(wildcard tests/*.c)) \
	$(patsubst tests/%.c,%.lst,$(wildcard tests/*.c)) \
	$(patsubst tests/%.s,%.bin,$(wildcard tests/*.s)) \
	$(patsubst tests/%.s,%.lst,$(wildcard tests/*.s)) \

# keep all intermediate files
.SECONDARY:

.PHONY: all
all: dir riscv-tests $(addprefix $(BUILD)/, $(BUILDS)) $(CBUILD)/startup.o \
	kernel/kernel.bin kernel/kernel.lst kernel/user.lst
# 	$(info BUILDING: $(addprefix $(BUILD)/, $(BUILDS)))

$(CBUILD)/startup.o: $(TESTS)/startup/startup.s
	$(CC) $(CFLAGS) -c $< -o $@

$(CBUILD)/%.o: $(TESTS)/%.c
	$(CC) $(CFLAGS) -c $< -o $@	

$(SBUILD)/%.o: $(TESTS)/%.s
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD)/%.elf: $(CBUILD)/%.o $(CBUILD)/startup.o $(TESTS)/prog.ld
	$(CC) $(CFLAGS) -T $(TESTS)/prog.ld $< $(CBUILD)/startup.o -o $@

$(BUILD)/%.elf: $(SBUILD)/%.o $(TESTS)/prog.ld
	$(CC) $(CFLAGS) -T $(TESTS)/prog.ld $< -o $@

$(BUILD)/%.bin: $(BUILD)/%.elf
	$(OBJCOPY) -O binary $< $@

$(BUILD)/%.lst: $(BUILD)/%.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@

dir:
	@mkdir -p $(BUILD)
	@mkdir -p $(CBUILD)
	@mkdir -p $(SBUILD)

.PHONY: emulate
emulate: all
	racket emulate.rkt

.PHONY: proof
proof: all
	raco test test.rkt

.PHONY: test
test: all
	@echo "----------------------"
	racket init.rkt
	@echo "----------------------"
	racket decode.rkt
	@echo "----------------------"
	racket fmt.rkt
	@echo "----------------------"
	racket execute.rkt
	@echo "----------------------"
	racket test.rkt
	@echo "----------------------"
	racket machine.rkt
	@echo "----------------------"
	racket emulate.rkt
	@echo "----------------------"

.PHONY: clean
clean: clean-riscv-tests
	rm -f *.elf *.o *.lst
	rm -rf build
	rm -f kernel/*.elf
	rm -f kernel/*.o
	rm -f kernel/*.bin
	rm -f kernel/*.lst
	rm -f riscv-tests/isa/*.bin
	rm -f riscv-tests/Makefile

.PHONY: qemu
qemu: kernel/kernel.elf
	$(QEMU) $(QEMUFLAGS) -kernel kernel/kernel.elf

.PHONY: qemu-gdb
qemu-gdb: kernel/kernel.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel kernel/kernel.elf

.PHONY: gdb
gdb: kernel/kernel.elf
	$(GDB) --data-directory=/usr/local/share/gdb kernel/kernel.elf

include riscv-tests.mk
include kernel.mk

