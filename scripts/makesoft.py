#!/usr/bin/env python

from __future__ import print_function
from plumbum import local, FG, cli
from plumbum.cmd import  git, cmake, make, chmod
import logging
from util import logfmt, TemporaryDirectory
import sys

logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

def downloadGithubArchive(ownerrepo, commit='master'):
    """Makes 'repo-<commit>' directory."""
    url = 'https://github.com/{ownerrepo}/archive/{commit}.tar.gz'.format(**locals())
    repo = ownerrepo.split('/')[1]
    (curl['-L', url] | tar['xz'])()
    return local.path(repo+'-'+commit)

def getCommitInfo(repo_path):
    with local.cwd(local.path(repo_path)):
        sha = git('rev-parse', '--short', 'HEAD')[:-1]
        date = git('show', '-s', '--format=%cd', '--date=short')[:-1]
    return (sha, date)

def downloadGithubRepo(ownerrepo, commit='master'):
    url = 'https://github.com/{ownerrepo}.git'.format(**locals())
    repo = ownerrepo.split('/')[1]
    if not local.path(repo).exists():
        git('clone', url)
    with local.cwd(repo):
        git('checkout', commit)
    return local.path(repo)

class MakeSoftware(cli.Application):
    """Software installer."""

    dest = cli.SwitchAttr(['-d', '--dest'], cli.ExistingDirectory, help="Root directory in which to install repo", envname='soft')
    commit = cli.SwitchAttr(['-c', '--commit'], help='GitHub hash commit. If omitted will get latest commit from the master branch.'
                             ,mandatory=False
                             ,default="master")

    def main(self, *args):
        if args:
            print("Unknown command {0!r}".format(args[0]))
            return 1
        if not self.nested_command:
            print("No command given")
            return 1   # error exit code

@MakeSoftware.subcommand("brainstools")
class BrainsTools(cli.Application):
    """Downloads and compiles BRAINSTools binaries. Output is 'BRAINSTools-bin-<hash>'."""

    def main(self):
        blddir = self.parent.dest / "BRAINSTools-build"
        with local.cwd(self.parent.dest):
            repo = downloadGithubRepo('BRAINSia/BRAINSTools', self.parent.commit)
        sha, date = getCommitInfo(repo)
        logging.info("Build code:")
        blddir.mkdir()
        with local.cwd(blddir):
            cmake(repo
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
        out = self.parent.dest / 'BRAINSTools-bin-'+sha
        symlink = self.parent.dest / 'BRAINSTools-bin-'+date
        (blddir / 'bin').move(out)
        (blddir / 'ANTs/Scripts/antsRegistrationSyN.sh').copy(out)
        chmod('a-w', out // '*')
        chmod('a-w', out)
        outdir.symlink(symlink)

def installTraining(repo, commit):
    archive = downloadGithubArchive('pnlbwh/'+repo, commit)
    archive.move(dest/repo)
    with local.cwd(dest/repo):
        from plumbum.cmd import bash
        bash('./mktrainingcsv.sh', '.')
        chmod('a-w', '*')

@MakeSoftware.subcommand("t2s")
class T2s(cli.Application):
    """Downloads t2 training set (has masks only). Makes '<dest>/trainingDataT2Masks"""
    def main(self):
        installTraining('trainingDataT2Masks', self.parent.commit)

@MakeSoftware.subcommand("t1s")
class T1s(cli.Application):
    """Downloads t1 training set. Has masks, amygdala-hippocampus (left/right), and cingulate (left/right). Makes '<dest>/trainingDataT1AHCC'"""
    def main(self):
        installTraining('trainingDataT1AHCC', self.parent.commit)

@MakeSoftware.subcommand("tractquerier")
class App(cli.Application):
    """Makes a read-only version of tract_querier. Output is '<dest>/tract_querier-<commit>'."""
    def main(self):
        with TemporaryDirectory() as tmpdir, local.cwd(tmpdir):
            repo = downloadGithubRepo('demianw/tract_querier', self.parent.commit)
            sha, date = getCommitInfo(repo)
            # save space
            (repo / 'doc').delete()
            (repo / '.git').delete()
            outdir = local.path(self.parent.dest / 'tract_querier-' + sha)
            if outdir.exists():
                logging.warning(outdir + ' already exists, quitting.')
                sys.exit(0)
            logging.info("Make '{outdir}'".format(**locals()))
            repo.move(outdir)
        chmod('-R', 'a-w', outdir)
        chmod('a-w', outdir)
        date_symlink = self.parent.dest / 'tract_querier-' + date
        outdir.symlink(date_symlink)

if __name__ == '__main__':
    MakeSoftware.run()
