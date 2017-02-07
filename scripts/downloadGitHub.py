#!/usr/bin/env python
from plumbum.cmd import curl, tar
from plumbum import local, cli
from util import logfmt

import logging
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

class App(cli.Application):
    DESCRIPTION='Download archive of a repo\'s master branch on github'
    owner = cli.SwitchAttr(['-o', '--owner'], help='github owner', default='pnlbwh')

    def main(self, repo):
        archive = 'https://github.com/{self.owner}/{repo}/archive/master.tar.gz'.format(**vars())
        scriptdir = local.path(__file__).dirname
        with local.cwd(scriptdir):
            (curl['-L', archive] | tar['xz'])()
            local.path(repo+'-master').move(repo)

if __name__ == '__main__':
    App.run()
