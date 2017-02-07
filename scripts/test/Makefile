.PHONY: epi b0 b0m ed t2atlas
epi: _data/dwiepi.nrrd
b0: _data/b0.nrrd
b0m: _data/b0masked.nrrd
ed: _data/dwied.nrrd
t2atlas: _data/t2atlas

T2s=$(addprefix ../trainingDataT2Masks/,01063-t2w.nrrd 01099-t2w.nrrd)
T2MASKs=$(addprefix ../trainingDataT2Masks/,01063-t2w-mask.nrrd 01099-t2w-mask.nrrd)
T1s=$(addprefix ../trainingDataT1AHCC/,01603-t2w.nrrd 01099-t2w.nrrd)
T1MASKs=$(addprefix ../trainingDataT1AHCC/,01603-t2w-mask.nrrd 01099-t2w-mask.nrrd)

# 006_ACT_001-AHCC-amyhipl.nrrd
# 006_ACT_001-AHCC-amyhipr.nrrd
# 006_ACT_001-AHCC-cingl.nrrd
# 006_ACT_001-AHCC-cingr.nrrd
# 006_ACT_001-t1w-realign.cent.nrrd
# 006_ACT_002-AHCC-amyhipl.nrrd
# 006_ACT_002-AHCC-amyhipr.nrrd
# 006_ACT_002-AHCC-cingl.nrrd
# 006_ACT_002-AHCC-cingr.nrrd
# 006_ACT_002-t1w-realign.cent.nrrd

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

_data/t2atlas: ../atlas.py _data $(T2s) $(T2MASKs)
	../atlas.py -g "$(T2s)" -l "$(T2MASKs)" -n t2atlasmask -t $(t2) -o $@

../trainingDataT2Masks/%:
	../downloadGitHub.py trainingDataT2Masks

../trainingDataT1AHCC/%:
	../downloadGitHub.py trainingDataT1AHCC


print-%  : ; @echo $* = $($*)
