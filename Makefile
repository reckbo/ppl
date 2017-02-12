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
	cp -r --no-clobber .config _config
	ln -s _config/Need.hs Need.hs
	ln -s _config/Paths.hs Paths.hs
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
