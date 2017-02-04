module Paths
  (outdir
  ,softwareDir
  ,given
  ) where

import System.FilePath ((</>))

--
-- Valid Given vars:
-- t1mask
-- t2mask
-- t2xcmask
-- t1xcmask

softwareDir = "/data/pnl/soft"
given = cte

cte = base "/data/pnl/DIAGNOSE_CTE_U01/TravelingHeads/"
	[("t2", "{case}/CTE_{case}_V_003/CTE_{case}_V_003-AXT2.nrrd")
	,("dwi","{case}/CTE_{case}_V_003/CTE_{case}_V_003-dwi.nrrd")
	]



u01_hcp = base "/data/pnl/U01_HCP_Psychosis/Protocol/Indiana"
          [("dwiHcpPos", "{case}/unprocessed/3T/Diffusion/{case}_3T_DWI_dir{num}_PA.nii.gz")
          ,("dwiHcpNeg", "{case}/unprocessed/3T/Diffusion/{case}_3T_DWI_dir{num}_AP.nii.gz")
          ,("dwi", "{case}/buddi/{case}_3T_dir98_PA_up_DRBUDDI_proc/{case}_3T_dir98_PA_up_DRBUDDI_SAVE_AFNI/DWI.nii.gz")
          ,("t1", "{case}/T1w/T1w.nii.gz")
          ,("t2", "{case}/T2w/T2w.nii.gz")
          ]


hcp2 = base "/data/pnl/soft/hcp/u01/"
      [
       ("dwiHcpPos", "{case}/{case}_3T_DWI_dir{num}_PA.nii.gz")
      ,("dwiHcpNeg", "{case}/{case}_3T_DWI_dir{num}_AP.nii.gz")
      ]


hcp = [("dwiHcpPos", "in/{case}/{num}PA.nii.gz")
      ,("dwiHcpNeg", "in/{case}/{num}AP.nii.gz")
      ,("t1", "in/{case}/{case}_3T_T1w_MPR1.nii.gz")
      ,("t2", "in/{case}/{case}_3T_T1w_SPC1.nii.gz")
      ]


intrust = base "/data/pnl/INTRuST/"
          [("dwi", "{case}/diff/{case}-dwi-Ed.nhdr")
          ,("dwimask", "{case}/diff/{case}-tensor-mask.nhdr")
          ,("t1", "{case}/raw/{case}-t1w.nhdr")
          ,("t2", "{case}/raw/{case}-t2w.nhdr")
          ,("freesurfer", "{case}/strct/{case}.freesurfer")
          ,("dwimask", "{case}/diff/{case}-tensor-mask.nhdr")
          ,("ukf", "{case}/diff/{case}.vtk")
          ]

local = [("dwi", "in/{case}/{case}.dwi-Ed.nrrd")
       ,("dwimask", "in/{case}/{case}-tensor-mask.nrrd")
       ,("freesurfer", "in/{case}/{case}.freesurfer")
       ]


base :: String -> [(String, FilePath)] -> [(String, FilePath)]
base dir list = map (fmap $ \f -> dir </> f) list
-- run :: [t] -> String -> [String] -> Action ()
-- run [] exe args = command_ [] "bsub" $ ["-K"
--                                     ,"-n", "8"
--                                     ,"-o", "%J.out"
--                                     ,"-e", "%J.err"
--                                     ,"-q", "big-multi"]
--                ++ [exe] ++ args

outdir = "_data"
