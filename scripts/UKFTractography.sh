#!/bin/bash -eu

repo=https://github.com/pnlbwh/ukftractography.git
dirSrc=UKFTractography
dirBld=UKFTractography-build

if [ ! -d "$dirSrc" ]; then 
    git clone $repo $dirSrc
else
    pushd "$dirSrc" 
    git pull origin
    popd
fi

[ -d "$dirBld" ] || mkdir "$dirBld"

cd "$dirBld" && cmake "../$dirSrc" && make
