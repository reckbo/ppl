f=$int/SetUpData.sh

case=003_GNX_022 && source $f
t1_1=$t1raw
t1mask_1=$t1rawmask
t2_1=$t2raw
t2mask_1=$t2rawmask

case=003_GNX_025 && source $f
t2_2=$t2raw
t2mask_2=$t2rawmask
t1_2=$t1raw
t1mask_2=$t1rawmask

case=003_GNX_007 && source $f
t2=$t2raw
for var in $(compgen -v); do export $var; done
