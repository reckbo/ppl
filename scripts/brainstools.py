#!/usr/bin/env python

from __future__ import print_function
from plumbum import local, FG, cli
from plumbum.cmd import  git, cmake, make
import logging
from util import logfmt
import sys

logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

class App(cli.Application):
    DESCRIPTION = "Downloads and compiles BRAINSTools binaries. Output is 'BRAINSTools-bin-<hash>'."
    prefix = cli.SwitchAttr('-d', cli.ExistingDirectory, help="Root directory in which to install repo", default=local.path('/data/pnl/soft'))
    githash = cli.SwitchAttr('-g', help='GitHub hash commit. If omitted will get latest commit from the master branch.')


    def main():
        srcdir = self.prefix / "BRAINSTools"
        blddir = self.prefix / "BRAINSTools-build"

        logging.info("Get source:")
        if not srcdir.exists():
            repo = 'https://github.com/BRAINSia/BRAINSTools.git'
            git("clone", repo, srcdir)
        else:
            with local.cwd(srcdir):
                git("fetch", "origin")
                if self.githash is not None:
                    git("checkout", args.githash)
                clone_hash = git("rev-parse", "--short", "HEAD")[:-1] # remove trailing \n

        logging.info("Build code:")
        blddir.mkdir()
        with local.cwd(blddir):
            cmake(srcdir
            ,"-DBRAINSTools_INSTALL_DEVELOPMENT=OFF"
            ,"-DBRAINSTools_MAX_TEST_LEVEL=0"
            ,"-DBRAINSTools_SUPERBUILD=ON"
            ,"-DBRAINSTools_USE_QT=OFF"
            ,"-DBRAINS_DEBUG_IMAGE_WRITE=OFF"
            ,"-DBUILD_STYLE_UTILS=OFF"
            ,"-DBUILD_TESTING=OFF"
            ,"-DCMAKE_BUILD_TYPE=Release"
            ,"-DCMAKE_COLOR_MAKEFILE=ON"
            ,"-DCMAKE_EXE_LINKER_FLAGS=' '"
            ,"-DCMAKE_EXE_LINKER_FLAGS_DEBUG="
            ,"-DCMAKE_EXE_LINKER_FLAGS_MINSIZEREL="
            ,"-DCMAKE_EXE_LINKER_FLAGS_RELEASE="
            ,"-DCMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO="
            ,"-DCMAKE_EXPORT_COMPILE_COMMANDS=OFF"
            ,"-DCMAKE_INSTALL_PREFIX:PATH=/usr/local"
            ,"-DCMAKE_MODULE_LINKER_FLAGS=' '"
            ,"-DCMAKE_MODULE_LINKER_FLAGS_DEBUG="
            ,"-DCMAKE_MODULE_LINKER_FLAGS_MINSIZEREL="
            ,"-DCMAKE_MODULE_LINKER_FLAGS_RELEASE="
            ,"-DCMAKE_MODULE_LINKER_FLAGS_RELWITHDEBINFO="
            ,"-DCMAKE_PROJECT_NAME:STATIC=SuperBuild_BRAINSTools"
            ,"-DCMAKE_SHARED_LINKER_FLAGS=' '"
            ,"-DCMAKE_SHARED_LINKER_FLAGS_DEBUG="
            ,"-DCMAKE_SHARED_LINKER_FLAGS_MINSIZEREL="
            ,"-DCMAKE_SHARED_LINKER_FLAGS_RELEASE="
            ,"-DCMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO="
            ,"-DCMAKE_SKIP_INSTALL_RPATH=NO"
            ,"-DCMAKE_SKIP_RPATH=NO"
            ,"-DCMAKE_STATIC_LINKER_FLAGS="
            ,"-DCMAKE_STATIC_LINKER_FLAGS_DEBUG="
            ,"-DCMAKE_STATIC_LINKER_FLAGS_MINSIZEREL="
            ,"-DCMAKE_STATIC_LINKER_FLAGS_RELEASE="
            ,"-DCMAKE_STATIC_LINKER_FLAGS_RELWITHDEBINFO="
            ,"-DCMAKE_USE_RELATIVE_PATHS=OFF"
            ,"-DCMAKE_VERBOSE_MAKEFILE=FALSE"
            ,"-DCOVERAGE_EXTRA_FLAGS=-l"
            ,"-DCTEST_SUBMIT_RETRY_COUNT=3"
            ,"-DCTEST_SUBMIT_RETRY_DELAY=5"
            ,"-DDART_TESTING_TIMEOUT=1500"
            ,"-DEXTERNAL_PROJECT_BUILD_TYPE=Release"
            ,"-DFORCE_EXTERNAL_BUILDS=OFF"
            ,"-DITK_VERSION_MAJOR=4"
            ,"-DSuperBuild_BRAINSTools_BUILD_DICOM_SUPPORT=ON"
            ,"-DSuperBuild_BRAINSTools_USE_CTKAPPLAUNCHER=OFF"
            ,"-DSuperBuild_BRAINSTools_USE_GIT_PROTOCOL=ON"
            ,"-DUSE_ANTS=ON"
            ,"-DUSE_AutoWorkup=OFF"
            ,"-DUSE_BRAINSABC=OFF"
            ,"-DUSE_BRAINSConstellationDetector=OFF"
            ,"-DUSE_BRAINSContinuousClass=OFF"
            ,"-DUSE_BRAINSCreateLabelMapFromProbabilityMaps=OFF"
            ,"-DUSE_BRAINSCut=OFF"
            ,"-DUSE_BRAINSDWICleanup=OFF"
            ,"-DUSE_BRAINSDemonWarp=OFF"
            ,"-DUSE_BRAINSFit=OFF"
            ,"-DUSE_BRAINSInitializedControlPoints=OFF"
            ,"-DUSE_BRAINSLabelStats=OFF"
            ,"-DUSE_BRAINSLandmarkInitializer=OFF"
            ,"-DUSE_BRAINSMultiModeSegment=OFF"
            ,"-DUSE_BRAINSMultiSTAPLE=OFF"
            ,"-DUSE_BRAINSMush=OFF"
            ,"-DUSE_BRAINSPosteriorToContinuousClass=OFF"
            ,"-DUSE_BRAINSROIAuto=OFF"
            ,"-DUSE_BRAINSResample=OFF"
            ,"-DUSE_BRAINSSnapShotWriter=OFF"
            ,"-DUSE_BRAINSStripRotation=OFF"
            ,"-DUSE_BRAINSSurfaceTools=OFF"
            ,"-DUSE_BRAINSTalairach=OFF"
            ,"-DUSE_BRAINSTransformConvert=OFF"
            ,"-DUSE_ConvertBetweenFileFormats=ON"
            ,"-DUSE_DWIConvert=ON"
            ,"-DUSE_DebugImageViewer=OFF"
            ,"-DUSE_GTRACT=OFF"
            ,"-DUSE_ICCDEF=OFF"
            ,"-DUSE_ImageCalculator=OFF"
            ,"-DUSE_ReferenceAtlas=OFF"
            ,"-DUSE_SYSTEM_DCMTK=OFF"
            ,"-DUSE_SYSTEM_ITK=OFF"
            ,"-DUSE_SYSTEM_SlicerExecutionModel=OFF"
            ,"-DUSE_SYSTEM_VTK=OFF"
            ,"-DVTK_GIT_REPOSITORY=git://vtk.org/VTK.git"
            )
            make['all'] & FG

        outbin = self.prefix / 'BRAINSTools-bin-'+clone_hash
        (blddir / 'bin').move(outbin)

if __name__ == '__main__':
    main()
