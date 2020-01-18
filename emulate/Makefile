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

BUILDDIR = build
TESTS = tests

BUILDS := \
	sum.bin sum.lst \
	store_and_load.bin store_and_load.lst \
	csr_test.bin csr_test.lst \

.PHONY: all
all: dir $(addprefix $(BUILDDIR)/, $(BUILDS))
	$(info BUILDING: $(addprefix $(BUILDDIR)/, $(BUILDS)))

$(BUILDDIR)/%.o: $(TESTS)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILDDIR)/%.o: $(TESTS)/%.s
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILDDIR)/%.elf: $(BUILDDIR)/%.o prog.ld
	$(CC) $(CFLAGS) -T prog.ld $< -o $@

$(BUILDDIR)/%.bin: $(BUILDDIR)/%.elf
	$(OBJCOPY) -O binary $< $@

$(BUILDDIR)/%.lst: $(BUILDDIR)/%.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@

dir:
	mkdir -p $(BUILDDIR)

.PHONY: emulate
emulate: all
	racket emulate.rkt

.PHONY: proof
proof: all
	raco test test.rkt

.PHONY: test
test: dir $(addprefix $(BUILDDIR)/, $(BUILDS))
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
	rm -f *.elf *.o
	rm -rf build

.PHONY: qemu
qemu: $(BUILDDIR)/sum.elf
	$(QEMU) $(QEMUFLAGS) -kernel $(BUILDDIR)/sum.elf

.PHONY: qemu-gdb
qemu-gdb: $(BUILDDIR)/sum.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel $(BUILDDIR)/sum.elf

.PHONY: gdb
gdb: $(BUILDDIR)/sum.elf
	$(GDB) --data-directory=/usr/local/share/gdb $(BUILDDIR)/sum.elf
