RISCV_TESTS = riscv-tests
RISCV_TESTS_ISA = riscv-tests/isa

RISCV_TESTS_BUILDS = $(patsubst riscv-tests/isa/%.dump, %.bin, $(wildcard riscv-tests/isa/*.dump))

# keep all intermediate files
.SECONDARY:

.PHONY: all-riscv-tests
all-riscv-tests: $(RISCV_TESTS) $(RISCV_TESTS)/Makefile
# 	$(addprefix $(RISCV_TESTS_ISA)/, $(RISCV_TESTS_BUILDS))

$(RISCV_TESTS_ISA)/%.bin: $(RISCV_TESTS_ISA)/%
	$(OBJCOPY) -O binary $< $@

$(RISCV_TESTS):
	@mkdir -p riscv-tests
	@git submodule update --init --recursive; 

$(RISCV_TESTS)/Makefile:
	@cd riscv-tests; \
		git submodule update --init --recursive; \
		autoconf; \
		./configure --prefix=$$RISCV/target; \
		make; \
		make install

	
.PHONY: clean-riscv-tests
clean-riscv-tests:
	rm -f $(RISCV_TESTS_ISA)/*.bin

