#!/usr/bin/env python
from plumbum.cmd import bsub
from plumbum import local, cli, bsub
from util import logfmt
from os import getpid

import logging
logging.basicConfig(level=logging.DEBUG, format=logfmt(__file__))

class App(cli.Application):
    DESCRIPTION='Start bsub job'
    numcores = cli.SwitchAttr('-n', int, help='number of CPU cores to use', default=8)
    jobname = cli.SwitchAttr('-j', help='job name', default=getpid())

    def main(self, cmd):
        bsub('-J', jobname
             ,'-o', "$*-%J.out"
             ,'-e', "$*-%J.err"
             ,'-q', "big-multi"
             ,'-n' '4'
             ,cmd)

if __name__ == '__main__':
    App.run()
