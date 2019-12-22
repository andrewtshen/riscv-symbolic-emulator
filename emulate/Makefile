PREFIX ?= riscv64-unknown-elf-
DEBUG ?= 1

QEMU := qemu-system-riscv64
QEMUFLAGS := -M virt -nographic -bios none

CC := $(PREFIX)gcc
GDB := $(PREFIX)gdb
OBJDUMP := $(PREFIX)objdump
OBJCOPY := $(PREFIX)objcopy
OBJDUMPFLAGS := --disassemble-all --source --section-headers --demangle

CFLAGS := -ffreestanding -nostdlib -O0 -mcmodel=medany
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

.PHONY: clean
clean:
	rm -f *.o *.lst *.elf

.PHONY: qemu
qemu: sum.elf
	$(QEMU) $(QEMUFLAGS) -sum sum.elf

.PHONY: qemu-gdb
qemu-gdb: sum.elf
	$(QEMU) $(QEMUFLAGS) -S -s -kernel sum.elf

.PHONY: gdb
gdb: sum.elf
	$(GDB) --data-directory=/usr/local/share/gdb sum.elf
