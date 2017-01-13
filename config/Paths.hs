module Paths
  (outdir
  ,softwareDir
  ,given
  ) where

-- import Development.Shake (command_, Action (..))

given = intrust

softwareDir = "/data/pnl/soft"

hcp2 = map (fmap ("/data/pnl/soft/hcp/u01/"++))
      [
       ("dwiHcpPos", "{case}/{case}_3T_DWI_dir{num}_PA.nii.gz")
      ,("dwiHcpNeg", "{case}/{case}_3T_DWI_dir{num}_AP.nii.gz")
      ]


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
          ,("freesurfer", "{case}/strct/{case}.freesurfer")
          ,("dwimask", "{case}/diff/{case}-tensor-mask.nhdr")
          ,("ukf", "{case}/diff/{case}.vtk")
          ]

outdir = "_data"

-- run :: [t] -> String -> [String] -> Action ()
-- run [] exe args = command_ [] "bsub" $ ["-K"
--                                     ,"-n", "8"
--                                     ,"-o", "%J.out"
--                                     ,"-e", "%J.err"
--                                     ,"-q", "big-multi"]
--                ++ [exe] ++ args
