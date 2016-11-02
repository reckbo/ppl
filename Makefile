.PHONY: all
all:
	stack build && stack exec ppl

.PHONY: clean
clean:
	rm -f *.out *.err core.*
