PREFIX ?= riscv64-unknown-elf-
DEBUG ?= 1

QEMU := qemu-system-riscv32
QEMUFLAGS := -M sifive_e -serial stdio

CC := $(PREFIX)gcc
GDB := $(PREFIX)gdb
NM := $(PREFIX)nm
OBJDUMP := $(PREFIX)objdump
OBJCOPY := $(PREFIX)objcopy
OBJDUMPFLAGS := --disassemble-all --source --section-headers --demangle

# no built-in rules and variables
MAKEFLAGS       += --no-builtin-rules --no-builtin-variables

BASE_CFLAGS     += -ffreestanding
BASE_CFLAGS     += -fno-stack-protector
BASE_CFLAGS     += -fno-strict-aliasing
# make it simpler for symbolic execution to track PC
BASE_CFLAGS     += -fno-jump-tables
# no unaligned memory accesses
BASE_CFLAGS     += -mstrict-align
BASE_CFLAGS     += -g -O0
BASE_CFLAGS     += -Wall -MD -MP
BASE_CFLAGS     += -nostdlib

CONFIG_CFLAGS   += -DCONFIG_NR_CPUS=$(CONFIG_NR_CPUS)
CONFIG_CFLAGS   += -DCONFIG_BOOT_CPU=$(CONFIG_BOOT_CPU)
CONFIG_CFLAGS   += -DCONFIG_DRAM_START=$(CONFIG_DRAM_START)
CONFIG_CFLAGS   += -DCONFIG_VERIFICATION=$(CONFIG_VERIFICATION)

CFLAGS          += $(BASE_CFLAGS) $(CONFIG_CFLAGS)
CFLAGS          += -mcmodel=medany
# no floating point
CFLAGS          += -mabi=ilp32
CFLAGS          += -ffunction-sections -fdata-sections
CFLAGS          += -fno-PIE
CFLAGS          += -I include
CFLAGS          += -march=rv32im

ifeq ($(DEBUG),1)
	CFLAGS += -g
endif

OBJS := \
	neg.o \

include test.mk

.DEFAULT_GOAL = all
.PHONY: all
all: kernel.elf kernel.lst kernel.bin kernel.asm kernel.map $(LEGOS_TESTS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $< -o $@

%.ll: %.c
	$(Q)$(MKDIR_P) $(@D)
	$(LLVM_CC) -o $@ -mno-sse -S -emit-llvm -fno-discard-value-names $(UBSAN_CFLAGS) -Wno-unused-command-line-argument -I include $(filter-out -g,$(BASE_CFLAGS)) $(CONFIG_CFLAGS) -DCONFIG_VERIFICATION_LLVM -c $<

kernel.elf: $(OBJS) kernel.ld
	$(CC) $(CFLAGS) -T kernel.ld $(OBJS) -o $@

%.lst: %.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@

# include zeros for bss in the binary
%.bin: %.elf
	$(OBJCOPY) -O binary --set-section-flags .bss=alloc,load,contents $< $@

# --prefix-addresses prints the complete address on each line
%.asm: %.elf
	$(OBJDUMP) -M no-aliases --prefix-addresses -w -f -d -z --show-raw-insn "$<" > "$@"

# sort addresses for *.map.rkt
%.map: %.elf
	$(NM) --print-size --numeric-sort "$<" > "$@"

.PHONY: clean
clean:
	rm -f *.o *.lst *.elf *.bin *.map *.asm

.PHONY: qemu
qemu: kernel.elf
	$(QEMU) $(QEMUFLAGS) -kernel kernel.elf

.PHONY: qemu-gdb
qemu-gdb: kernel.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel kernel.elf

.PHONY: gdb
gdb: kernel.elf
	$(GDB) kernel.elf