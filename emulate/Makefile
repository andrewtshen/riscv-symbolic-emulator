PREFIX ?= riscv64-unknown-elf-
DEBUG ?= 1

QEMU := qemu-system-riscv64
QEMUFLAGS := -M virt -nographic -bios none

CC := $(PREFIX)gcc
GDB := $(PREFIX)gdb
OBJDUMP := $(PREFIX)objdump
OBJCOPY := $(PREFIX)objcopy
OBJDUMPFLAGS := --disassemble-all --source --section-headers --demangle

CFLAGS := -ffreestanding -nostdlib -O0 -mcmodel=medany -march=rv64i
ifeq ($(DEBUG),1)
	CFLAGS += -g
endif

OBJS := \
	sum.o \

.PHONY: all
all: sum.bin sum.lst

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $< -o $@

sum.elf: $(OBJS) sum.ld
	$(CC) $(CFLAGS) -T sum.ld $(OBJS) -o $@

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.lst: %.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@

.PHONE: test
test: sum.bin
	@echo "----------------------"
	@echo "> starting load.rkt"
	racket load.rkt
	@echo "> ending load.rkt"
	@echo "----------------------"
	@echo "> starting decode.rkt"
	racket decode.rkt
	@echo "> ending decode.rkt"
	@echo "----------------------"
	@echo "> starting emulate.rkt"
	racket emulate.rkt
	@echo "> ending emulate.rkt"
	@echo "----------------------"

.PHONY: clean
clean:
	rm -f *.o *.lst *.elf

.PHONY: qemu
qemu: sum.elf
	$(QEMU) $(QEMUFLAGS) -kernel sum.elf

.PHONY: qemu-gdb
qemu-gdb: sum.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel sum.elf

.PHONY: gdb
gdb: sum.elf
	$(GDB) --data-directory=/usr/local/share/gdb sum.elf
