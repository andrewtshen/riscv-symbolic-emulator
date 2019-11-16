MAKEFLAGS += -r
MAKEFLAGS += -R

PREFIX ?= arm-none-eabi-
DEBUG ?= 1

CC := $(PREFIX)gcc
LD := $(PREFIX)ld
OBJCOPY := $(PREFIX)objcopy
OBJDUMPFLAGS := --disassemble-all --source --section-headers --demangle

OBJDUMP := $(PREFIX)objdump

HEADERFILES := $(wildcard *.h)

OBJS := \
    sign.o

CFLAGS=-O0 -c -g -mcpu=cortex-m3 -mthumb -nostdlib -Wall -Wextra -fno-builtin -ffreestanding -Wno-unused-but-set-variable

.PRECIOUS: %.o

.PHONY: all
all: sign.elf sign.lst sign.bin

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.elf: $(OBJS)
	$(LD) -o $@ $^

%.lst: %.elf
	$(OBJDUMP) $(OBJDUMPFLAGS) $< > $@


%.o: %.c $(HEADERFILES)
	$(CC) $(CFLAGS) -o $@ $<

%.o: %.s $(HEADERFILES)
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.bin *.elf *.lst
