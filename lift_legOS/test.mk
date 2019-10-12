# Testing/Proof

LEGOS_TESTS := \
		test.rkt \
		spec.rkt \

# verify-legOS: $(LEGOS_TESTS)
# 	$(RACO_TEST) $^

# verify-legOS-%: monitors/legOS/verif/%.rkt
# 	$(RACO_TEST) $^

$(LEGOS_TESTS): | \
	kernel.asm.rkt \
	kernel.map.rkt \
	kernel.globals.rkt \

%.globals.rkt: %.elf
	$(Q)echo "#lang reader serval/lang/dwarf" > $@~
	$(QUIET_GEN)$(OBJDUMP) --dwarf=info $< >> $@~
	$(Q)mv $@~ $@

%.asm.rkt: %.asm
	$(QUIET_GEN)echo "#lang reader serval/riscv/objdump" > $@~ && \
		cat $< >> $@~
	$(Q)mv $@~ $@

%.map.rkt: %.map
	$(QUIET_GEN)echo "#lang reader serval/lang/nm" > $@~ && \
		cat $< >> $@~
	$(Q)mv "$@~" "$@"

test: $(LEGOS_TESTS)
	raco test --check-stderr --table --timeout 1200 --jobs 4 test.rkt spec.rkt

