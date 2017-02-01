.PHONY: all
all:
	stack build

.PHONY: clean
clean:
	rm -f *.out *.err core.*

.PHONY: setupdata
setupdata:
	stack exec ppl

%:
	stack exec ppl -- --metadata=_data/$* $*

%-bsub4:
	bsub -J $* -o "$*-%J.out" -e "$*-%J.err" -q "big-multi" -n 4 stack exec ppl -- --metadata=_data/$* $*

%-bsub8:
	bsub -J $* -o "$*-%J.out" -e "$*-%J.err" -q "big-multi" -n 8 stack exec ppl -- --metadata=_data/$* $*

%-bsub16:
	bsub -J $* -o "$*-%J.out" -e "$*-%J.err" -q "big-multi" -n 16 stack exec ppl -- --metadata=_data/$* $*
