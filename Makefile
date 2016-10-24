.PHONY: all
all:
	stack build && stack exec ppl

.PHONY: clean
clean:
	rm *.out *.err core.*
