.PHONY: all
all:
	stack build

.PHONY: clean
clean:
	rm -f *.out *.err core.*

run:
	stack exec ppl

%:
	stack exec ppl -- --metadata=_data/$* $*
