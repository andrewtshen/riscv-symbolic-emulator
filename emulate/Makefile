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

CFLAGS := -ffreestanding -nostdlib -O0 -mcmodel=medany -march=rv64i -mabi=lp64
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

.SECONDARY:

.PHONY: all
all: dir $(addprefix $(BUILD)/, $(BUILDS)) $(CBUILD)/startup.o
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
test: dir $(addprefix $(BUILD)/, $(BUILDS))
	@echo "----------------------"
	racket load.rkt
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
clean:
	rm -f *.elf *.o
	rm -rf build

.PHONY: qemu
qemu: $(BUILD)/pmp.elf
	$(QEMU) $(QEMUFLAGS) -kernel $(BUILD)/pmp.elf

.PHONY: qemu-gdb
qemu-gdb: $(BUILD)/pmp.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel $(BUILD)/pmp.elf

.PHONY: gdb
gdb: $(BUILD)/pmp.elf
	$(GDB) --data-directory=/usr/local/share/gdb $(BUILD)/pmp.elf
