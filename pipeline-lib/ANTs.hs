module ANTs
  (computeRigid
  ,computeWarp
  ,applyTransforms
  ,upsample
  ,freesurferToDwi
  ,freesurferToDwiWithMasks
  ,makeRigidMask
  ,defaultParams
  ,initialStage
  ,rigidStage
  ,affineStage
  ,synStage
  ,warpStages
  ,Metric (..)
  ,warpMI
  ,warpCC
  ) where

import           Control.Monad             (when)
import           Control.Monad.Extra       (whenM)
import           Data.Foldable             (traverse_)
import           Data.List                 (intercalate)
import           Data.Maybe                (fromMaybe)
import           Development.Shake         (Action, CmdOption (AddEnv), cmd,
                                            liftIO, unit, withTempDir)
import           Development.Shake.Command
import           System.Directory          as IO (copyFile,
                                                  createDirectoryIfMissing,
                                                  doesDirectoryExist)
import           System.Environment        (lookupEnv)
import           System.FilePath           ((</>))
import           System.FilePath           (combine, takeExtensions, (<.>),
                                            (</>))
import           System.IO.Temp            (withSystemTempDirectory,
                                            withSystemTempFile)
import           System.Process            (callProcess)
import qualified Teem                      (center, gzip, isNrrd)
import           Util                      (convertImage, extractB0, maskImage)

type Moving = FilePath
type Fixed = FilePath
type Warp = FilePath

getAntsPath :: FilePath -> IO FilePath
getAntsPath ""
  = do envPath <- fmap (fromMaybe $ error "getAnts: ANTSPATH not set, set it or call function with a path") (lookupEnv "ANTSPATH")
       case envPath of
         "" -> error "getAnts: ANTSPATH is set to an empty path."
         _  -> getAntsPath envPath
getAntsPath path = do
  whenM (not <$> IO.doesDirectoryExist path)
    (error $ "getAnts: the path " ++ path ++ " does not exist")
  return path


-- TODO replace ANTS with newer antsRegistration
computeRigid antspath moving fixed outtxt
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "Affine.txt"
    callProcess (antspath </> "ANTS") ["3"
                                      ,"-m", "MI["++fixed++","++moving++",1,32]"
                                      ,"-i", "0",
                                       "-o", pre, "--do-rigid"]
    IO.copyFile affine outtxt

computeWarp :: FilePath -> Moving -> Fixed -> Warp -> Action ()
computeWarp antspath moving fixed outwarp =
  withTempDir $
  \tmpdir ->
    do let pre = tmpdir </> "ants"
           affine = pre ++ "0GenericAffine.mat"
           warp = pre ++ "1Warp.nii.gz"
       command_ []
                (antspath </> "antsRegistration") $
         warpCC moving fixed [pre]
       command_ []
                (antspath </> "ComposeMultiTransform")
                ["3",outwarp,"-R",fixed,warp,affine]

applyTransforms antspath interpolation transforms moving fixed out =
  command_ [] (antspath </> "antsApplyTransforms") $
    ["-d", "3"
    ,"-i", moving
    ,"-o", out
    ,"-r", fixed
    ,"-t"] ++ transforms
    ++ (if null interpolation then [] else ["--interpolation", interpolation])

upsample antspath spacings img out
  = callProcess (antspath </> "ResampleImageBySpacing")
  ["3", img, out, unwords . map show $ spacings]


makeRigidMask antsPath mask moving fixed out
  = withTempDir $ \tmpdir -> do
        let [tmpxfm, tmpmask, tmpmoving] = map (combine tmpdir) ["xfm.txt", "mask.nrrd", "moving.nrrd"]
        liftIO $ convertImage mask tmpmask
        liftIO $ convertImage moving tmpmoving
        liftIO $ traverse_ Teem.center [tmpmask, tmpmoving]
        liftIO $ computeRigid antsPath tmpmoving fixed tmpxfm
        applyTransforms antsPath "NearestNeighbor" [tmpxfm] tmpmask fixed out
        liftIO $ when (Teem.isNrrd out) (Teem.gzip out)

freesurferToDwi :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
freesurferToDwi antsPath mridir bse t1 t2 outdir = do
  liftIO $ createDirectoryIfMissing True outdir
  fshome <- liftIO $ fromMaybe (error "freesurferToDwi: Set FREESURFER_HOME") <$> lookupEnv "FREESURFER_HOME"
  let brain = outdir </> "brain.nii.gz"
      wmparc = outdir </> "wmparc.nii.gz"
      bse = outdir </> "bse" <.> (takeExtensions bse) -- TODO constrain to nrrd/nii
      fsToT1_rigid = outdir </> "fsToT1-rigid.txt"
      t1ToT2_rigid = outdir </> "t1ToT2-rigid.txt"
      t2ToDwi_warp = outdir </> "t2ToDwi-warp.nii.gz"
      wmparcInDwi = outdir </> "wmparc-in-dwi" <.> (takeExtensions bse) -- TODO
  unit $ cmd (AddEnv "SUBJECTS_DIR" "") (fshome </> "bin" </> "mri_vol2vol")
    ["--mov", mridir </> "brain.mgz"
    ,"--targ", mridir </> "brain.mgz"
    ,"--regheader"
    ,"--o", brain]
  unit $ cmd (AddEnv "SUBJECTS_DIR" "")  (fshome </> "bin" </> "mri_label2vol")
    ["--seg", mridir </> "wmparc.mgz"
    ,"--temp", mridir </> "brain.mgz"
    ,"--regheader", mridir </> "wmparc.mgz"
    ,"--o", wmparc]
  -- Make upsampled DWI b0
  liftIO $ upsample antsPath [1,1,1] bse bse
  liftIO $ computeRigid antsPath brain t1 fsToT1_rigid
  liftIO $ computeRigid antsPath t1 t2 t1ToT2_rigid
  computeWarp antsPath t2 bse t2ToDwi_warp
  applyTransforms antsPath "NearestNeighbor"
    [t2ToDwi_warp, t1ToT2_rigid, fsToT1_rigid]
    wmparc bse wmparcInDwi
  unit $ cmd "ConvertBetweenFileFormats" [wmparcInDwi, wmparcInDwi, "short"]

freesurferToDwiWithMasks antsPath mridir dwi dwimask t1 t1mask t2 t2mask outdir
  = withTempDir $ \tmpdir -> do
  let [bsemasked, t1masked, t2masked] = map (tmpdir </>) ["bsemasked.nii.gz"
                                                         ,"t1masked.nii.gz"
                                                         ,"t2masked.nii.gz"]
  Util.extractB0 dwi (tmpdir </> "bse.nii.gz")
  liftIO $ maskImage (tmpdir </> "bse.nii.gz") dwimask bsemasked
  liftIO $ maskImage t1 t1mask t1masked
  liftIO $ maskImage t2 t2mask t2masked
  freesurferToDwi antsPath mridir bsemasked t1masked t2masked outdir

--------------------------------------------------------------------------------
--- From antsRegistrationSyN.sh

data Metric = CC | MI
  deriving (Eq, Show)

initialStage f m = ["--initial-moving-transform"
                   ,"["++f++","++m++",1]"
                   ]

rigidConvergence = "[1000x500x250x100,1e-6,10]"
rigidShrinkFactors = "8x4x2x1"
rigidSmoothingSigmas = "3x2x1x0vox"
rigidStage f m = ["--transform", "Rigid[0.1]"
                   ,"--metric", "MI["++f++","++m++",1,32,Regular,0.25]"
                   ,"--convergence", rigidConvergence
                   ,"--shrink-factors", rigidShrinkFactors
                   ,"--smoothing-sigmas", rigidSmoothingSigmas
                   ]

affineConvergence = "[1000x500x250x100,1e-6,10]"
affineShrinkFactors = "8x4x2x1"
affineSmoothingSigmas = "3x2x1x0vox"
affineStage f m = ["--transform", "Affine[0.1]"
                   ,"--metric", "MI["++f++","++m++",1,32,Regular,0.25]"
                   ,"--convergence", affineConvergence
                   ,"--shrink-factors", affineShrinkFactors
                   ,"--smoothing-sigmas", affineSmoothingSigmas
                   ]

synMetrics CC f m = ["--metric", "CC["++f++","++m++",1,4]"]
synMetrics MI f m = ["--metric", "MI["++f++","++m++",1,32,Regular,0.25]"]
synConvergence = "[100x70x50x20,1e-6,10]"
synShrinkFactors = "8x4x2x1"
synSmoothingSigmas = "3x2x1x0vox"
synStage metric f m = ["--transform", "SyN[0.1,3,0]"]
                     ++ (synMetrics metric f m)
                     ++ ["--convergence", synConvergence
                        ,"--shrink-factors", synShrinkFactors
                        ,"--smoothing-sigmas", synSmoothingSigmas
                        ]

warpStages metric m f = initialStage f m
                     ++ rigidStage f m
                     ++ affineStage f m
                     ++ synStage metric f m

defaultParams = ["--verbose", "1"
                ,"--dimensionality", "3"
                ,"--float", "1"
                ,"--interpolation", "Linear"
                ,"--use-histogram-matching", "0"
                ,"--winsorize-image-intensities", "[0.005,0.995]"
                ]

warpCC moving fixed outputs
  = defaultParams ++ ["--output", showOutputs outputs] ++ warpStages CC moving fixed

warpMI moving fixed outputs
  = defaultParams ++ ["--output", showOutputs outputs] ++ warpStages MI moving fixed

showOutputs xs = "[" ++ s ++ "]"
  where s = intercalate "," xs
