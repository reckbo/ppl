d=cte

T2_CASEIDS=01063 01099
T2_PREFIXES=$(addprefix ../trainingDataT2Masks/, $(T2_CASEIDS))
T2S=$(addsuffix -t2w.nrrd, $(T2_PREFIXES))
T2_MASKS=$(addsuffix -t2w-mask.nrrd, $(T2_PREFIXES))

T1_CASEIDS=006_ACT_001 006_GNX_020
T1_PREFIXES=$(addprefix ../trainingDataT1AHCC/, $(T1_CASEIDS))
T1S=$(addsuffix -t1w.nrrd, $(T1_PREFIXES))
T1_MASKS=$(addsuffix -t1w-mask.nrrd, $(T1_PREFIXES))
T1_CINGRS=$(addsuffix -AHCC-cingr.nrrd, $(T1_PREFIXES))

.PHONY: epi b0 b0m ed t2atlas
epi: _data/dwiepi.nrrd
b0: _data/b0.nrrd
b0m: _data/b0masked.nrrd
ed: _data/dwied.nrrd
t2atlas: $(addprefix _data/t2atlas/, atlasmask0.nrrd atlasmask1.nrrd)
t1atlas: $(addprefix _data/t1atlas/, atlasmask0.nrrd atlasmask1.nrrd atlascingr0.nrrd atlascingr1.nrrd)

_data:
	mkdir $@

_data/dwiepi.nrrd: ../epi.py _data
	$(call check_defined, dwi, dwimask, t2, t2mask)
	../epi.py --dwi $(dwi) --dwimask $(dwimask) --t2 $(t2) --t2mask $(t2mask) --out $@

_data/b0masked.nrrd: ../bse.py _data
	$(call check_defined, dwi, dwimask)
	../bse.py -m $(dwimask) -i $(dwi) -o $@

_data/b0.nrrd: ../bse.py _data
	$(call check_defined, dwi)
	../bse.py -i $(dwi) -o $@

_data/dwied.nrrd: ../eddy.py _data
	$(call check_defined, dwi)
	../eddy.py -i $(dwi) -o $@

_data/t2atlas/%.nrrd: ../atlas.py _data $(T2S) $(T2_MASKS)
	$(call check_defined, t2)
	../atlas.py -g "$(T2S)" -l "$(T2_MASKS)" -n t2atlasmask -t $(t2) -o $@

_data/t1atlas/%.nrrd: ../atlas.py _data $(T1S) $(T1_MASKS) $(T1_CINGRS)
	$(call check_defined, t1)
	../atlas.py -g "$(T1S)" -l "$(T1_MASKS) $(T1_CINGRS)" -n "atlasmask atlascingr" -t $(t1) -o $@

../trainingDataT2Masks/%:
	../downloadGitHub.py trainingDataT2Masks

../trainingDataT1AHCC/%:
	../downloadGitHub.py trainingDataT1AHCC


%-bsub24:
	../bsub.py -j $* -n 24 "make $*"

%-bsub16:
	../bsub.py -j $* -n 16 "make $*"

%-bsub8:
	../bsub.py -j $* -n 8 "make $*"

%-bsub4:
	../bsub.py -j $* -n 4 "make $*"

# debug target: e.g `make print-t2`
print-%  : ; @echo $* = $($*)

# Check that given variables are set and all have non-empty values,
# die with an error otherwise.
#
# Params:
#   1. Variable name(s) to test.
#   2. (optional) Error message to print.
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
      $(error Undefined $1$(if $2, ($2))))
