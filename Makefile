.PHONY: all
all:
	stack build && stack exec ppl

.PHONY: clean
clean:
	rm -f *.out *.err core.*

%:
	stack exec ppl -- --metadata=_data/$* $*
%-bsub:
	bsub -J $* -o "$*-%J.out" -e "$*-%J.err" -q "big-multi" -n 8 stack exec ppl -- --metadata=_data/$* $*
