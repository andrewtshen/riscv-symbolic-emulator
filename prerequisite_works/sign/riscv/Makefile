PREFIX ?= riscv64-unknown-elf-
DEBUG ?= 1

CC := $(PREFIX)gcc
OBJDUMP := $(PREFIX)objdump
OBJDUMPFLAGS := --disassemble-all --source --section-headers --demangle


CFLAGS := -march=rv32im -mabi=ilp32 -ffreestanding -nostdlib -O0
ifeq ($(DEBUG),1)
	CFLAGS += -g
endif

OBJS := \
    sign.o

.PHONY: all
all: sign.elf sign.lst

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $< -o $@

sign.elf: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

%.lst: %.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@

.PHONY: clean
clean:
	rm -f *.o *.lst *.elf
