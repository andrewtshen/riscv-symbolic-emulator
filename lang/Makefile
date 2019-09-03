MAKEFLAG += -r
MAKEFLAG += -R

FILENAME=test
GCC=gcc

.PHONY: all
all: $(FILENAME)

$(FILENAME): $(FILENAME).o
	$(GCC) $< -o $@	 

$(FILENAME).o: $(FILENAME).toy
	python main.py $(FILENAME).toy
	llc -filetype=obj $(FILENAME).ll

.PHONY: run
run: $(FILENAME)
	@./$(FILENAME)||true

.PHONY: clean
clean: 
	rm -f *.pyc *.ll *.o
	find . -type f -name "test"  -delete