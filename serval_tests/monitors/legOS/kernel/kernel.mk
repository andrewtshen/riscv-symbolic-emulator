LEGOS_KERNEL_ELF       := $(O)/monitors/legOS/kernel.elf

LEGOS_KERNEL_OBJS      := $(call object,$(wildcard monitors/legOS/kernel/*.c monitors/legOS/kernel/*.S))

$(LEGOS_KERNEL_ELF): $(KERNEL_LDS) $(KERNEL_BOOT_OBJS) $(KERNEL_OBJS) $(LEGOS_KERNEL_OBJS)
	$(QUIET_LD)$(LD) -o $@ $(LDFLAGS) -T $^
	$(Q)$(OBJDUMP) -S $@ > $(basename $@).asm
