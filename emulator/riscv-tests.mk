RISCV_TESTS = riscv-tests
RISCV_TESTS_ISA = riscv-tests/isa

RISCV_TESTS_BUILDS = $(patsubst riscv-tests/isa/%.dump, %.bin, $(wildcard riscv-tests/isa/*.dump))

# keep all intermediate files
.SECONDARY:

.PHONY: all-riscv-tests
all-riscv-tests: riscv-tests-dir $(RISCV_TESTS)/Makefile
	@make $(addprefix $(RISCV_TESTS_ISA)/, $(RISCV_TESTS_BUILDS))

$(RISCV_TESTS_ISA)/%.bin: $(RISCV_TESTS_ISA)/% $(RISCV_TESTS)/Makefile
	$(OBJCOPY) -O binary $< $@

.PHONY: riscv-tests-dir
riscv-tests-dir:
	@mkdir -p riscv-tests
	@git submodule update --init --recursive 

$(RISCV_TESTS)/Makefile:
	@cd riscv-tests; \
		git submodule update --init --recursive; \
		autoconf; \
		./configure --prefix=$$RISCV/target; \
		make
	
.PHONY: clean-riscv-tests
clean-riscv-tests:
	rm -f $(RISCV_TESTS_ISA)/*.bin
