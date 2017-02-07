#!/usr/bin/env python

#from __future__ import print_function
from util import TemporaryDirectory
from plumbum import local, FG, cli
from plumbum.cmd import  git, rm, chmod
import logging
from util import logfmt
import sys

logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

class App(cli.Application):
    DESCRIPTION = "Makes a read-only version of tract_querier with a \
particular commit. Output is 'tract_querier-<hash>'."

    prefix = cli.SwitchAttr('-d', cli.ExistingDirectory, help="Root directory in which to install repo", default=local.path('/data/pnl/soft'))
    githash = cli.SwitchAttr('-g', help='GitHub hash commit. If omitted will get latest commit from the master branch.')

    def main(self):
        repo = 'https://github.com/demianw/tract_querier.git'
        with TemporaryDirectory() as tmpdir:
            clone = local.path(tmpdir) / "tract_querier"
            if not self.githash:
                git("clone", "--depth", "1", repo, clone)
            else:
                git("clone", repo, clone)
            clone_hash = git("rev-parse", "--short", "HEAD")[:-1] # remove trailing \n
            # save 70M of space
            rm('-r', clone / 'doc')
            rm('-r', clone / '.git')
            out = self.prefix / "tract_querier-"+clone_hash
            clone.move(out)
            chmod('-R', 'a-w', out)

if __name__ == '__main__':
    App.run()
