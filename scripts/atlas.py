#!/usr/bin/env python
from __future__ import print_function
import operator
from util import logfmt, TemporaryDirectory
from util.ants import ComposeMultiTransform, antsRegistrationSyN_sh, antsApplyTransforms
from plumbum import local, cli, FG
from plumbum.cmd import unu, mkdir
import sys
from collections import namedtuple
from itertools import count, izip_longest

import logging
logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

def grouper(iterable, n, fillvalue=None):
        "Collect data into fixed-length chunks or blocks"
        # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx
        args = [iter(iterable)] * n
        return izip_longest(fillvalue=fillvalue, *args)

def computeWarp(image, target, out):
    with TemporaryDirectory() as tmpdir:
        tmpdir = local.path(tmpdir)
        pre = tmpdir / 'ants'
        warp = pre + '1Warp.nii.gz'
        affine = pre + '0GenericAffine.mat'
        antsRegistrationSyN_sh['-m', image, '-f', target, '-o', pre, '-n', 32] & FG
        ComposeMultiTransform('3', out, '-R', target, warp, affine)

def applyWarp(moving, warp, reference, out, interpolation='Linear'):
    '''Interpolation options:
    Linear
    NearestNeighbor
    MultiLabel[<sigma=imageSpacing>,<alpha=4.0>]
    Gaussian[<sigma=imageSpacing>,<alpha=1.0>]
    BSpline[<order=3>]
    CosineWindowedSinc
    WelchWindowedSinc
    HammingWindowedSinc
    LanczosWindowedSinc
    GenericLabel[<interpolator=Linear>]
    '''
    antsApplyTransforms('-d', '3'
                        ,'-i', moving
                        ,'-t', warp
                        ,'-r', reference
                        ,'-o', out
                        ,'--interpolation', interpolation)

class App(cli.Application):
    DESCRIPTION="Makes atlas image/labelmap pairs for a target image."
    images = cli.SwitchAttr(['-g','--images'],   help='list of images' ,mandatory=True)
    labels = cli.SwitchAttr(['-l','--labels'],   help='list of labelmap images' ,mandatory=True)
    names  = cli.SwitchAttr(['-n','--names'], list=True, help='list of label names' ,mandatory=True)
    target = cli.SwitchAttr(['-t','--target'], cli.ExistingFile, help='target image',mandatory=True)
    out    = cli.SwitchAttr(['-o', '--out'], cli.NonexistentPath, help='output directory', mandatory=True)


    def main(self):
        images = self.images.split()
        labels = self.labels.split()
        quotient, remainder = divmod(len(labels), len(images))
        TrainingPoint = namedtuple('TrainingPoint', 'idx image labels warp atlas atlaslabels')

        if remainder != 0:
            logging.error('Wrong number of labelmaps, must be a multiple of number of images (' + str(len(images)) + '). Instead there is a remainder of ' + str(remainder))
            sys.exit(1)

        if quotient != len(self.names):
            log.error('Wrong number of names, must match number of labelmap sets')
            sys.exit(1)

        labelgroups = grouper(labels, quotient)

        with TemporaryDirectory() as tmpdir:
            tmpdir = local.path(tmpdir)

            trainingpoints = [TrainingPoint(idx = idx
                                            ,image = image
                                            ,labels = labelgroup
                                            ,warp = tmpdir / "warp"+str(idx)+'.nii.gz'
                                            ,atlas = self.out / 'atlas' + str(idx) + '.nrrd'
                                            ,atlaslabels = [self.out/n+str(idx)+'.nrrd' for n in self.names]
                                            )
                              for idx, image, labelgroup in
                              zip(count(), images, labelgroups)]

            mkdir('-p', self.out)

            logging.info('Compute transforms from images to target and apply')
            for pt in trainingpoints:
                computeWarp(pt.image, self.target, pt.warp)
                applyWarp(pt.image, pt.warp, self.target, pt.atlas)
                for label, atlaslabel in zip(pt.labels, pt.atlaslabels):
                    applyWarp(label, pt.warp, self.target, atlaslabel, interpolation='NearestNeighbor')

	    assert(self.out.exists)
            logging.info('Made ' + self.out)

if __name__ == '__main__':
    App.run()
