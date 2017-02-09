from __future__ import print_function
from os.path import abspath, exists, dirname, join
import os
import os as _os
import logging
import warnings as _warnings
from tempfile import mkdtemp
from plumbum import cli


logger = logging.getLogger()

def logfmt(scriptname):
    return '%(asctime)s ' + scriptname + ' %(levelname)s  %(message)s'

# def run(cmd):
#     logger.info(' '.join(cmd))
#     check_call(cmd)

def getext(path):
    if path.endswith('.nii.gz'):
	return '.nii.gz'
    return os.path.splitext(path)[1]

class BrainsToolsScript(cli.Application):
    @cli.switch('--brainstools', cli.ExistingDirectory, help='Path to BRAINSTools binaries, will override PATH and ANTSPATH', mandatory=False)
    def brainstools(self, path):
        if path:
            import os
            os.environ['PATH'] = path + ':' + os.environ['PATH']
            os.environ['ANTSPATH'] = path

# class BrainsTools(object):
#     def __init__(self, path):
#         self.antsApplyTransforms = k wh


class TemporaryDirectory(object):
    """Create and return a temporary directory.  This has the same
    behavior as mkdtemp but can be used as a context manager.  For
    example:

        with TemporaryDirectory() as tmpdir:
            ...

    Upon exiting the context, the directory and everything contained
    in it are removed.
    """

    def __init__(self, suffix="", prefix="tmp", dir=None):
        self._closed = False
        self.name = None # Handle mkdtemp raising an exception
        self.name = mkdtemp(suffix, prefix, dir)

    def __repr__(self):
        return "<{} {!r}>".format(self.__class__.__name__, self.name)

    def __enter__(self):
        return self.name

    def cleanup(self, _warn=False):
        if self.name and not self._closed:
            try:
                self._rmtree(self.name)
            except (TypeError, AttributeError) as ex:
                # Issue #10188: Emit a warning on stderr
                # if the directory could not be cleaned
                # up due to missing globals
                if "None" not in str(ex):
                    raise
                print("ERROR: {!r} while cleaning up {!r}".format(ex, self,),
                      file=_sys.stderr)
                return
            self._closed = True
            if _warn:
                self._warn("Implicitly cleaning up {!r}".format(self),
                           ResourceWarning)

    def __exit__(self, exc, value, tb):
        self.cleanup()

    def __del__(self):
        # Issue a ResourceWarning if implicit cleanup needed
        self.cleanup(_warn=True)

    # XXX (ncoghlan): The following code attempts to make
    # this class tolerant of the module nulling out process
    # that happens during CPython interpreter shutdown
    # Alas, it doesn't actually manage it. See issue #10188
    _listdir = staticmethod(_os.listdir)
    _path_join = staticmethod(_os.path.join)
    _isdir = staticmethod(_os.path.isdir)
    _islink = staticmethod(_os.path.islink)
    _remove = staticmethod(_os.remove)
    _rmdir = staticmethod(_os.rmdir)
    _warn = _warnings.warn

    def _rmtree(self, path):
        # Essentially a stripped down version of shutil.rmtree.  We can't
        # use globals because they may be None'ed out at shutdown.
        for name in self._listdir(path):
            fullname = self._path_join(path, name)
            try:
                isdir = self._isdir(fullname) and not self._islink(fullname)
            except OSError:
                isdir = False
            if isdir:
                self._rmtree(fullname)
            else:
                try:
                    self._remove(fullname)
                except OSError:
                    pass
        try:
            self._rmdir(path)
        except OSError:
            pass

#===================================================================================================
# Module hack: ``from util.scripts import bse``
#===================================================================================================
import sys
from types import ModuleType
from plumbum import local

class LocalModule(ModuleType):
    """The module-hack that allows us to use ``from util.scripts import script_py``"""
    __all__ = ()  # to make help() happy
    __package__ = __name__
    def __getattr__(self, name):
        scriptname = name.replace('_', '.')
        scriptdir = abspath(join(dirname(__file__), '..'))
        filename = join(scriptdir, scriptname)
        if not exists(filename):
            raise AttributeError(filename)
        return local[filename]
    __path__ = []
    __file__ = __file__

scripts = LocalModule(__name__ + ".scripts", LocalModule.__doc__)
sys.modules[scripts.__name__] = scripts

del sys
del ModuleType
del LocalModule


#===================================================================================================
# Module hack: ``from util.antspath import antsRegistration``
#===================================================================================================
import sys
from types import ModuleType

class LocalModule(ModuleType):
    """The module-hack that allows us to use ``from util.scripts import script_py``"""
    __all__ = ()  # to make help() happy
    __package__ = __name__
    def __getattr__(self, name):
        antspath = os.environ.get('ANTSPATH',None)
        scriptname = name.replace('_', '.')
        filename = join(antspath, scriptname)
        if not antspath or not exists(filename):
            raise AttributeError(name)
        return local[filename]
    __path__ = []
    __file__ = __file__

antspath = LocalModule(__name__ + ".antspath", LocalModule.__doc__)
sys.modules[antspath.__name__] = antspath

del sys
del ModuleType
del LocalModule
