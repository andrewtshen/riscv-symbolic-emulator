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

BUILDDIR = build
TESTS = tests

BUILDS := \
	add.bin add.lst \
	store_and_load.bin store_and_load.lst \
	csr_test.bin csr_test.lst \
	sub.bin sub.lst \
	test.bin test.lst \
	pmp.bin pmp.lst

.PHONY: all
all: dir $(addprefix $(BUILDDIR)/, $(BUILDS))
	$(info BUILDING: $(addprefix $(BUILDDIR)/, $(BUILDS)))

$(BUILDDIR)/%.o: $(TESTS)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILDDIR)/%.o: $(TESTS)/%.s
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILDDIR)/%.elf: $(BUILDDIR)/%.o $(TESTS)/prog.ld
	$(CC) $(CFLAGS) -T $(TESTS)/prog.ld $< -o $@

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
qemu: $(BUILDDIR)/sum.elf
	$(QEMU) $(QEMUFLAGS) -kernel $(BUILDDIR)/sum.elf

.PHONY: qemu-gdb
qemu-gdb: $(BUILDDIR)/sum.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel $(BUILDDIR)/sum.elf

.PHONY: gdb
gdb: $(BUILDDIR)/sum.elf
	$(GDB) --data-directory=/usr/local/share/gdb $(BUILDDIR)/sum.elf
