f=$int/SetUpData.sh
case=003_GNX_007 && source $f
t2=$t2raw
t1=$t1raw
t2mask=$t2rawmask
t1mask=$t1rawmask
for var in $(compgen -v); do export $var; done
