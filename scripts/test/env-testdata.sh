source $testdata/SetUpData.sh
for var in $(compgen -v); do export $var; done
