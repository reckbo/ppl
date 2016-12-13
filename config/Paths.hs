module Paths
  (outdir
  ,given
  ) where


given = intrust


hcp = [("dwiHcpPos", "in/{case}/{num}PA.nii.gz")
      ,("dwiHcpNeg", "in/{case}/{num}AP.nii.gz")
      ,("t1", "in/{case}/{case}_3T_T1w_MPR1.nii.gz")
      ,("t2", "in/{case}/{case}_3T_T1w_SPC1.nii.gz")
      ]

misc = [("dwi", "in/{case}/{case}.dwi-Ed.nrrd")
       ,("dwimask", "in/{case}/{case}-tensor-mask.nrrd")
       ,("freesurfer", "in/{case}/{case}.freesurfer")
       ]

intrust = map (fmap ("/data/pnl/INTRuST/"++))
          [("dwi", "{case}/diff/{case}-dwi-Ed.nhdr")
          ,("dwimask", "{case}/diff/{case}-tensor-mask.nhdr")
          ,("t1", "{case}/raw/{case}-t1w.nhdr")
          ,("t2", "{case}/raw/{case}-t2w.nhdr")
          ]

outdir = "_data"