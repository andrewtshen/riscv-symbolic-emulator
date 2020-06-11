# Testing/Proof

asmprog_TESTS := \
		test.rkt \
# 		spec.rkt \

# verify-asmprog: $(asmprog_TESTS)
# 	$(RACO_TEST) $^

# verify-asmprog-%: monitors/asmprog/verif/%.rkt
# 	$(RACO_TEST) $^

$(asmprog_TESTS): | \
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

test: $(asmprog_TESTS)
	raco test --check-stderr --table --timeout 1200 --jobs 4 test.rkt

