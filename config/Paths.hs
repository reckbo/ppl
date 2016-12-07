module Paths where

import           System.FilePath ((</>))

given = misc

misc = [("dwi", "in/{case}/{case}.dwi-Ed.nrrd")
        ,("dwimask", "in/{case}/{case}-tensor-mask.nrrd")
        ,("freesurfer", "in/{case}/{case}.freesurfer")
        ]

intrust = map (fmap ("/data/pnl/INTRuST" </>))
          [("dwi", "{case}/diff/{case}-dwi-Ed.nhdr")
          ,("dwimask", "{case}/diff/{case}-tensor-mask.nhdr")
          ,("t1", "{case}/raw/{case}-t1w.nhdr")
          ,("t2", "{case}/raw/{case}-t2w.nhdr")
          ]

outdir = "_data"
