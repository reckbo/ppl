case=BWH && source $cte/SetUpData.sh
source ./software.sh
for var in $(compgen -v); do export $var; done
