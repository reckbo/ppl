.PHONY: all clean setupdata config run
all:
	stack build
clean:
	rm -f *.out *.err core.*
setupdata:
	stack exec ppl
configclean: ;
	rm -rf _config
	rm Need.hs Paths.hs
config:
	test -d _config || mkdir _config
	cp --no-clobber .config/* _config/
	test -h Need.hs || ln -s _config/Need.hs Need.hs
	test -h Paths.hs || ln -s _config/Paths.hs Paths.hs
	@echo Done
run: ; make 003_GNX_007
bsub: ; make 003_GNX_007-bsub8

%:
	stack exec ppl -- -d --metadata=_data/$* $*
%-bsub4:
	bsub -J $* -o "$*-%J.out" -e "$*-%J.err" -q "big-multi" -n 4 stack exec ppl -- --metadata=_data/$* $*
%-bsub8:
	bsub -J $* -o "$*-%J.out" -e "$*-%J.err" -q "big-multi" -n 8 stack exec ppl -- --metadata=_data/$* $*
%-bsub16:
	bsub -J $* -o "$*-%J.out" -e "$*-%J.err" -q "big-multi" -n 16 stack exec ppl -- --metadata=_data/$* $*
