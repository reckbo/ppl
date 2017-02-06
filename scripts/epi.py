#!/usr/bin/env python

from __future__ import print_function
from os.path import basename, splitext, abspath, exists, dirname, join
from os import getpid
from util import logfmt, TemporaryDirectory, getext
from util.scripts import bse_py, antsApplyTransformsDWI_sh
from util.ants import antsRegistrationSyN_sh, antsApplyTransforms, antsRegistration
from plumbum import local, cli
from plumbum.cmd import unu

import logging
logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

class App(cli.Application):

    debug = cli.Flag(['-d', '--debug'], help='Debug, don\'t delete temporary directory')
    dwi = cli.SwitchAttr('--dwi', cli.ExistingFile, help='DWI (nrrd)')
    dwimask = cli.SwitchAttr('--dwimask', cli.ExistingFile, help='DWI mask (nrrd)')
    t2 = cli.SwitchAttr('--t2', cli.ExistingFile, help='T2w (nrrd)')
    t2mask = cli.SwitchAttr('--t2mask', cli.ExistingFile, help='T2w mask (nrrd)')
    out = cli.SwitchAttr(['-o', '--out'], cli.NonexistentPath, help='EPI corrected DWI')

    def main(self):
        with TemporaryDirectory() as tmpdir:
            tmpdir = local.path(tmpdir)
            bse           = tmpdir / "maskedbse.nrrd"
            t2masked      = tmpdir / "maskedt2.nrrd"
            t2inbse       = tmpdir / "t2inbse.nrrd"
            epiwarp       = tmpdir / "epiwarp.nii.gz"
            t2tobse_rigid = tmpdir / "t2tobse_rigid"

            logging.info('1. Extract and mask the DWI b0')
            bse_py('-m', self.dwimask ,'-i', self.dwi ,'-o', bse)

            logging.info("2. Mask the T2")
            unu("3op", "ifelse", self.t2mask, self.t2, "0", "-o", t2masked)

            logging.info(
                "3. Compute a rigid registration from the T2 to the DWI baseline")
            antsRegistrationSyN_sh("-d", "3"
                                   ,"-f", bse
                                   ,"-m", t2masked
                                   ,"-t", "r"
                                   ,"-o", tmpdir / "t2tobse_rigid")
            antsApplyTransforms("-d", "3"
                                ,"-i", t2masked
                                ,"-o", t2inbse
                                ,"-r", bse
                                ,"-t", tmpdir / "t2tobse_rigid0GenericAffine.mat")

            logging.info("4. Compute 1d nonlinear registration from the DWI to the T2 along the phase direction")
            moving = bse
            fixed  = t2inbse
            pre    = tmpdir / "epi"
            dwiepi = tmpdir / "dwiepi"+getext(self.out)
            antsRegistration("-d", "3"
                             ,"-m", "cc["+fixed+","+moving+",1,2]"
                             ,"-t", "SyN[0.25,3,0]"
                             ,"-c", "50x50x10"
                             ,"-f", "4x2x1"
                             , "-s", "2x1x0"
                             ,"--restrict-deformation", "0x1x0"
                             ,"-v", "1"
                             ,"-o", pre)

            local.path(pre+"0Warp.nii.gz").move(epiwarp)

            logging.info("5. Apply warp to the DWI")
            antsApplyTransformsDWI_sh(self.dwi, self.dwimask, epiwarp, dwiepi)

            if getext(dwiepi) == '.nhdr':
                unu("save","-e","gzip","-f","nrrd","-i",dwiepi,self.out)
            else:
                dwiepi.move(self.out)

            if self.debug:
                tmpdir.move("epidebug-"+getpid())

if __name__ == '__main__':
    App.run()
