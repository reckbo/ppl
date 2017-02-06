.PHONY: epi b0 b0m ed
epi: _data/dwiepi.nrrd
b0: _data/b0.nrrd
b0m: _data/b0masked.nrrd
ed: _data/dwied.nrrd

_data:
	mkdir $@

_data/dwiepi.nrrd: ../epi.py _data
	../epi.py --dwi $(dwi) --dwimask $(dwimask) --t2 $(t2) --t2mask $(t2mask) --out $@

_data/b0masked.nrrd: ../bse.py _data
	../bse.py -m $(dwimask) -i $(dwi) -o $@

_data/b0.nrrd: ../bse.py _data
	../bse.py -i $(dwi) -o $@

_data/dwied.nrrd: ../eddy.py _data
	../eddy.py -i $(dwi) -o $@
