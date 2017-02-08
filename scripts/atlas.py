#!/usr/bin/env python
from __future__ import print_function
from util import logfmt, TemporaryDirectory
import util
from plumbum import local, cli, FG
from itertools import izip_longest
import pandas as pd

import logging
logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

def grouper(iterable, n, fillvalue=None):
        "Collect data into fixed-length chunks or blocks"
        # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx
        if n == 1:
                return [iterable]
        args = [iter(iterable)] * n
        return izip_longest(fillvalue=fillvalue, *args)

def computeWarp(image, target, out):
    from util.antspath import ComposeMultiTransform, antsRegistrationSyN_sh
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
    from util.antspath import antsApplyTransforms
    antsApplyTransforms('-d', '3'
                        ,'-i', moving
                        ,'-t', warp
                        ,'-r', reference
                        ,'-o', out
                        ,'--interpolation', interpolation)

def intersperse(seq, value):
    res = [value] * (2 * len(seq) - 1)
    res[::2] = seq
    return res

def makeAtlases(target, trainingTable, outdir, mabs=False):
    outdir = local.path(outdir)

    from plumbum.cmd import mkdir
    mkdir('-p', outdir)

    logging.info('Create atlases: compute transforms from images to target and apply')
    for idx, r in trainingTable.iterrows():
        warp = outdir / 'warp{idx}.nii.gz'.format(**locals())
        atlas = outdir / 'atlas{idx}.nii.gz'.format(**locals())
        computeWarp(r['image'], target, warp)
        applyWarp(r['image'], warp, target, atlas)
        for labelname, label in r.iloc[1:].iteritems():
            atlaslabel = outdir / '{labelname}{idx}.nii.gz'.format(**locals())
            applyWarp(label, warp, target, atlaslabel, interpolation='NearestNeighbor')

    if mabs:
        from plumbum.cmd import unu, ConvertBetweenFileFormats, AverageImages
        for labelname in list(trainingTable)[1:]:
            out = outdir / labelname + '.nrrd'
            labelmaps = outdir // labelname+'*'
            with TemporaryDirectory() as tmpdir:
                nii = tmpdir / 'mabs.nii.gz'
                AverageImages('3', nii, '0', *labelmaps)
                ConvertBetweenFileFormats(nii, out)
            unu['2op', 'gt', out, '0.5'] | \
                unu['save', '-e', 'gzip', '-f', 'nrrd', '-o', out] & FG


class App(cli.Application):
    DESCRIPTION="Makes atlas image/labelmap pairs for a target image."
    images = cli.SwitchAttr(['-g','--images'], help='list of images in quotations, e.g. "img1.nrrd img2.nrrd"' ,mandatory=True)
    labels = cli.SwitchAttr(['-l','--labels'], help='list of labelmap images in quotations, e.g. "mask1.nrrd mask2.nrrd cingr1.nrrd cingr2.nrrd"' ,mandatory=True)
    names  = cli.SwitchAttr(['-n','--names'], help='list of names for generated labelmaps, e.g. "atlasmask atlascingr"' ,mandatory=True)
    target = cli.SwitchAttr(['-t','--target'], cli.ExistingFile, help='target image',mandatory=True)
    mabs   = cli.Flag('--mabs', help='Also create predicted labelmap(s) by averaging the atlas labelmaps')
    out    = cli.SwitchAttr(['-o', '--out'], cli.NonexistentPath, help='output directory', mandatory=True)

    def main(self):
        images = self.images.split()
        labels = self.labels.split()
        labelnames = self.names.split()
        quotient, remainder = divmod(len(labels), len(images))
        if remainder != 0:
            logging.error('Wrong number of labelmaps, must be a multiple of number of images (' + str(len(images)) + '). Instead there is a remainder of ' + str(remainder))
            sys.exit(1)
        if quotient != len(labelnames):
            logging.error('Wrong number of names, must match number of labelmap training sets: ' + str(quotient))
            sys.exit(1)
        labelcols = grouper(labels, quotient)
        trainingTable = pd.DataFrame(dict(zip(labelnames, labelcols) + [('image', images)]))
        makeAtlases(self.target, trainingTable, self.out, self.mabs)
        logging.info('Made ' + self.out)

if __name__ == '__main__':
    App.run()
