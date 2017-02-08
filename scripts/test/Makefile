ifeq ($(soft),)
  $(error Environment variable 'soft' (where BRAINSTools binary directories are) is not set)
endif
T2_CASEIDS=01063 01099
T2_PREFIXES=$(addprefix ../_trainingDataT2Masks/, $(T2_CASEIDS))
T2S=$(addsuffix -t2w.nrrd, $(T2_PREFIXES))
T2_MASKS=$(addsuffix -t2w-mask.nrrd, $(T2_PREFIXES))

T1_CASEIDS=006_ACT_001 006_GNX_020
T1_PREFIXES=$(addprefix ../_trainingDataT1AHCC/, $(T1_CASEIDS))
T1S=$(addsuffix -t1w-realign.cent.nrrd, $(T1_PREFIXES))
T1_MASKS=$(addsuffix -t1w-realign-mask.nrrd, $(T1_PREFIXES))
T1_CINGRS=$(addsuffix -AHCC-cingr.nrrd, $(T1_PREFIXES))

.PHONY: all clean epi b0 b0m ed t2atlas t1atlas
epi: _data/dwiepi.nrrd
b0: _data/b0.nrrd
b0m: _data/b0masked.nrrd
ed: _data/dwied.nrrd
t2atlas: $(addprefix _data/t2atlas/, mask0.nrrd mask1.nrrd)
# t1atlas: $(addprefix _data/t1atlas/, mask0.nrrd mask1.nrrd cingr0.nrrd cingr1.nrrd)
t1atlas: $(addprefix _data/t1atlas/, mask0.nrrd mask1.nrrd)

clean:
	rm -r _data

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

_data/t2atlas/%0.nrrd	 _data/t2atlas/%1.nrrd: ../atlas.py _data $(T2S) $(T2_MASKS)
	$(call check_defined, t2)
	../atlas.py --mabs -g "$(T2S)" -l "$(T2_MASKS)" -n mask -t $(t2) -o $@

_data/t1atlas/%0.nrrd  _data/t1atlas/%1.nrrd: ../atlas.py _data $(T1S) $(T1_MASKS) $(T1_CINGRS)
	$(call check_defined, t1)
	../atlas.py -g "$(T1S)" -l "$(T1_MASKS) $(T1_CINGRS)" -n "mask cingr" -t $(t1) -o $@

../_trainingDataT2Masks/%.nrrd ../_trainingDataT2Masks/%-mask.nrrd:
	../downloadGitHub.py trainingDataT2Masks
	mv trainingDataT2Masks $@

%-bsub24:
	../bsub.py -j $* -n 24 "make $*"

%-bsub16:
	../bsub.py -j $* -n 16 "make $*"

%-bsub8:
	../bsub.py -j $* -n 8 "make $*"

%-bsub4:
	../bsub.py -j $* -n 4 "make $*"

print-%  : ; @echo $* = $($*)

# Check that given variables are set and all have non-empty values,
# die with an error otherwise.
# Params:
#   1. Variable name(s) to test.
#   2. (optional) Error message to print.
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
      $(error Undefined $1$(if $2, ($2))))
