.PHONY: all
all: epi

.PHONY: epi
epi: _data/dwiepi.nrrd

_data/dwiepi.nrrd:
	../epi.py --dwi $(dwi) --dwimask $(dwimask) --t2 $(t2) --t2mask $(t2mask) --out $@
