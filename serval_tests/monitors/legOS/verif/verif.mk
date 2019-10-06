LEGOS_TESTS := \
        monitors/legOS/verif/test.rkt \

verify-legOS: $(LEGOS_TESTS)
	$(RACO_TEST) $^

verify-legOS-%: monitors/legOS/verif/%.rkt
	$(RACO_TEST) $^

$(LEGOS_TESTS): | \
        $(O)/monitors/legOS.asm.rkt \
        $(O)/monitors/legOS.map.rkt \
        $(O)/monitors/legOS.globals.rkt \
        $(O)/monitors/legOS.ll.rkt \
        $(O)/monitors/legOS/verif/asm-offsets.rkt \

$(O)/monitors/legOS.ll: $(O)/monitors/legOS/main.ll
	$(QUIET_GEN)$(LLVM_LINK) $^ | $(LLVM_OPT) -o $@~ $(LLVM_OPTFLAGS) -S
	$(Q)mv $@~ $@

PHONY           += verify-legOS
