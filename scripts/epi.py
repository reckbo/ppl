#!/usr/bin/env python

from __future__ import print_function
import argparse
from os.path import basename, splitext, abspath, exists, dirname, join
import sys
from util import checkArgs, run, checkAntsPath, logfmt, TemporaryDirectory
import logging

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
    checkAntsPath()
    print("b")

    with TemporaryDirectory() as tmpdir:
        bse      = join(tmpdir, "maskedbse.nrrd")
        t2masked = join(tmpdir, "maskedt2.nrrd")
        t2inbse  = join(tmpdir, "t2inbse.nrrd")
        epiwarp  = join(tmpdir, "epiwarp.nii.gz")

        logging.info('1. Extract and mask the DWI b0')
        run([join(SCRIPTDIR, 'bse.py')
             ,'-m', args.dwimask
             ,'-i', args.dwi
             ,'-o', bse])



if __name__ == '__main__':
    main()
