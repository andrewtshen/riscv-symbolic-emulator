LEGOS_ELF      := $(O)/monitors/legOS.elf

LEGOS_OBJS     := $(call object,$(wildcard monitors/legOS/*.c monitors/legOS/*.S))
LEGOS_OBJS     += $(O)/monitors/legOS/kernel.bin.o

include monitors/legOS/kernel/kernel.mk
include monitors/legOS/verif/verif.mk

$(LEGOS_ELF): $(BIOS_LDS) $(BIOS_BOOT_OBJS) $(BIOS_OBJS) $(KERNEL_OBJS) $(LEGOS_OBJS)
	$(QUIET_LD)$(LD) -o $@ $(LDFLAGS) -T $^

qemu-legos: $(LEGOS_ELF)
	$(QEMU) $(QEMU_OPTS) -kernel $<

spike-legos: $(LEGOS_ELF)
	$(SPIKE) $(SPIKE_OPTS) $<

ALL             += $(LEGOS_ELF)

PHONY           += qemu-legos spike-legos
