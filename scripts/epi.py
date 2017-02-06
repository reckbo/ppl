#!/usr/bin/env python

from __future__ import print_function
import argparse
from os.path import basename, splitext, abspath, exists, dirname, join
import sys
from util import checkArgs, run, runAnts, getAntsPath, logfmt, TemporaryDirectory, getext
import shutil
import logging
import os

logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))
SCRIPTDIR=dirname(__file__)

def main():
    argparser = argparse.ArgumentParser(
        description="")

    argparser.add_argument('-d'
                           ,'--debug'
                           , help='Debug, don\'t delete temporary directory'
                           , required=False)
    argparser.add_argument('--dwi'
                           , help='DWI (nrrd)'
                           , required=True)
    argparser.add_argument('--dwimask'
                           , help='DWI mask (nrrd)'
                           , required=True)
    argparser.add_argument('--t2'
                           , help='T2w (nrrd)'
                           , required=True)
    argparser.add_argument('--t2mask'
                           , help='T2w mask (nrrd)'
                           , required=True)
    argparser.add_argument('-o'
                           ,'--out'
                           , help='Epi corrected DWI'
                           , required=True)

    args = argparser.parse_args()
    checkArgs(args, ignoreArgs=["debug", "out"])
    antspath = getAntsPath()

    with TemporaryDirectory() as tmpdir:
        bse      = join(tmpdir, "maskedbse.nrrd")
        t2masked = join(tmpdir, "maskedt2.nrrd")
        t2inbse  = join(tmpdir, "t2inbse.nrrd")
        epiwarp  = join(tmpdir, "epiwarp.nii.gz")
        t2tobse_rigid  = join(tmpdir, "t2tobse_rigid")

        logging.info('1. Extract and mask the DWI b0')
        run([join(SCRIPTDIR, 'bse.py')
             ,'-m', args.dwimask
             ,'-i', args.dwi
             ,'-o', bse])

        logging.info("2. Mask the T2")
        run(["unu", "3op", "ifelse", args.t2mask, args.t2, "0", "-o", t2masked])

        logging.info(
            "3. Compute a rigid registration from the T2 to the DWI baseline")
        runAnts(antspath, ["antsRegistrationSyN.sh"
                           , "-d", "3"
                           , "-f", bse
                           , "-m", t2masked
                           , "-t", "r"
                           , "-o", join(tmpdir, "t2tobse_rigid")])
        runAnts(antspath, ["antsApplyTransforms"
                           , "-d", "3"
                           , "-i", t2masked
                           , "-o", t2inbse
                           , "-r", bse
                           , "-t", join(tmpdir,"t2tobse_rigid0GenericAffine.mat")])

        logging.info("4. Compute 1d nonlinear registration from the DWI to the T2 along the phase direction")
        moving=bse
        fixed=t2inbse
        pre=join(tmpdir, "epi")
        dwiepi=join(tmpdir,"dwiepi"+getext(args.out))
        runAnts(antspath, ["antsRegistration", "-d", "3"
                           ,"-m", "cc["+fixed+","+moving+",1,2]"
                           ,"-t", "SyN[0.25,3,0]"
                           ,"-c", "50x50x10"
                           ,"-f", "4x2x1"
                           , "-s", "2x1x0"
                           ,"--restrict-deformation", "0x1x0"
                           ,"-v", "1"
                           ,"-o", pre])

        shutil.move(pre+"0Warp.nii.gz", epiwarp)

        logging.info("5. Apply warp to the DWI")
	run([join(SCRIPTDIR,"antsApplyTransformsDWI.sh"), args.dwi, args.dwimask, epiwarp, dwiepi])

        if getext(dwiepi) == '.nhdr':
	   unu("save","-e","gzip","-f","nrrd","-i",dwiepi,args.out)
        else:
           shutil.move(dwiepi, args.out)

        if args.debug:
            debugdir = "epidebug-"+os.getpid()
            os.makedirs(debugdir)
            shutil.move(tmpdir, debugdir)

if __name__ == '__main__':
    main()
