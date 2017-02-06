#!/usr/bin/env python

from __future__ import print_function
from past.builtins import basestring
import argparse
from os.path import dirname
import sys
import operator
from util import logfmt, checkArgs
import logging
from plumbum import local
from plumbum.cmd import unu

logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))
SCRIPTDIR=dirname(__file__)

def read_hdr(nrrd):
    hdr, stderr = Popen(['unu', 'head', nrrd], stdout=PIPE,
                           stderr=PIPE).communicate()
    return hdr

def get_grad_dirs(hdr):
    return [map(float, line.split(b'=')[1].split())
            for line in hdr.splitlines()
            if b'DWMRI_gradient' in line]

def get_bval(hdr):
    for line in hdr.splitlines():
        if b'b-value' in line:
            return float(line.split(b'=')[1])

def get_b0_index(hdr):
    bval = get_bval(hdr)
    bvals = [norm(gdir)*bval for gdir in get_grad_dirs(hdr)]
    idx, min_bval  = min(enumerate(bvals), key=operator.itemgetter(1))
    logger.info("Found B0 of " + str(min_bval) + " at index " + str(idx))
    return idx


def norm(vector):
    return sum([v**2 for v in vector])


def main():
    argparser = argparse.ArgumentParser(
        description="Extracts the baseline (b0) from a nrrd DWI.  Assumes \
        the diffusion volumes are indexed by the last axis.")

    argparser.add_argument('-m'
                           ,'--mask'
                           , help='DWI mask'
                           , required=False)

    argparser.add_argument('-i'
                           ,'--infile'
                           , help='DWI nrrd image'
                           , required=True)

    argparser.add_argument('-o'
                           ,'--out'
                           , help='B0 nrrd image'
                           , required=True)

    args = argparser.parse_args()
    checkArgs(args, ["out"])

    dwi = args.infile
    dwimask = args.mask
    hdr = read_hdr(dwi)
    idx = get_b0_index(hdr)

    slicecmd = unu["slice", "-a", "3", "-p", str(idx) ,"-i", dwi]
    maskcmd = unu["3op", "ifelse", "-w", "1", dwimask, "-", "0"]
    gzipcmd = unu["save", "-e", "gzip", "-f", "nrrd", "-o", args.out]

    if dwimask:
        (slicecmd | maskcmd | gzipcmd)()
    else:
        (slicecmd | gzipcmd)()

if __name__ == '__main__':
    main()
