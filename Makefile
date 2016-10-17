all:
	stack build && stack exec pipeline

clean:
	rm *.out *.err core.*
