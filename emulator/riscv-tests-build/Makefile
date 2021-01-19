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
	$(patsubst %.elf,%.bin,$(wildcard *.elf)) \
	$(patsubst %.elf,%.lst,$(wildcard *.elf)) \

# keep all intermediate files
.SECONDARY:

.PHONY: all
all: dir $(addprefix $(BUILD)/, $(BUILDS))
	echo $(BUILDS)

$(BUILD)/%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

$(BUILD)/%.lst: %.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@

dir:
	@mkdir -p $(BUILD)

.PHONY: clean
clean:
	rm -rf build

.PHONY: qemu
qemu: kernel/kernel.elf
	$(QEMU) $(QEMUFLAGS) -kernel kernel/kernel.elf

.PHONY: qemu-gdb
qemu-gdb: kernel/kernel.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel kernel/kernel.elf

.PHONY: gdb
gdb: kernel/kernel.elf
	$(GDB) --data-directory=/usr/local/share/gdb kernel/kernel.elf

