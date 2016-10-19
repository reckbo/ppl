all:
	stack build && stack exec ppl

clean:
	rm *.out *.err core.*
