import os.path
import sys
import subprocess
from typing import Optional
from setuptools import Extension, setup
from setuptools.errors import ExecError, PlatformError
from setuptools.command.build_ext import build_ext
from distutils.errors import DistutilsSetupError
from distutils.spawn import find_executable

ext_modules = [
    Extension(name="fib._binding", sources=["src/fib/binding.i"]),
]


class build_hs_ext(build_ext):
    cabal: Optional[str] = None
    runhaskell: Optional[str] = None

    def find_runhaskell(self):
        if self.runhaskell is None:
            self.runhaskell = find_executable("runhaskell")
            if self.runhaskell is None:
                raise ExecError("could not find executable 'runhaskell'")
        return self.runhaskell

    def find_cabal(self):
        if self.cabal is None:
            self.cabal = find_executable("cabal")
            if self.cabal is None:
                raise ExecError("could not find executable 'cabal'")
        return self.cabal

    def cabal_configure(self, ext: Extension):
        args = [self.find_runhaskell(), "Setup.hs", "configure"]
        args.append(self._cabal_component_name(ext))
        args.extend(
            f"--extra-lib-dirs={os.path.abspath(dir)}" for dir in self.library_dirs
        )
        args.extend(
            f"--extra-include-dirs={os.path.abspath(dir)}" for dir in self.include_dirs
        )
        args.append(
            f"--extra-prog-path={os.path.dirname(os.path.abspath(sys.executable))}"
        )
        args.append(f"--prefix={os.path.abspath(self.build_temp)}")
        args.append(f"--libdir=")
        args.append(f"--dynlibdir=")
        args.append(f"--datadir=")
        args.append(f"--docdir=")
        args.append(f"--builddir={os.path.abspath(self.build_temp)}")
        print(" ".join(args))
        subprocess.call(args)

    def cabal_build(self, ext: Extension):
        args = [self.find_runhaskell(), "Setup.hs", "build"]
        args.append(self._cabal_component_name(ext))
        args.append(f"--builddir={self.build_temp}")
        print(" ".join(args))
        subprocess.call(args)

    def cabal_copy(self, ext: Extension):
        args = [self.find_runhaskell(), "Setup.hs", "copy"]
        args.append(self._cabal_component_name(ext))
        args.append(f"--builddir={self.build_temp}")
        args.append(f"--destdir={self.build_temp}")
        print(" ".join(args))
        subprocess.call(args)
        ext_source = self._cabal_component_library_path(ext)
        ext_target = self.get_ext_fullpath(ext.name)
        self.mkpath(os.path.dirname(ext_target))
        self.copy_file(ext_source, ext_target)

    def build_extension(self, ext):
        sources = ext.sources
        if sources is None or not isinstance(sources, (list, tuple)):
            raise DistutilsSetupError(
                "in 'ext_modules' option (extension '%s'), "
                "'sources' must be present and must be "
                "a list of source filenames" % ext.name
            )
        # sort to make the resulting .so file build reproducible
        sources = sorted(sources)

        # First, scan the sources for SWIG definition files (.i), run
        # SWIG on 'em to create .c files, and modify the sources list
        # accordingly.
        sources = self.swig_sources(sources, ext)

        # Next, run build the sources with Cabal.
        self.cabal_configure(ext)
        self.cabal_build(ext)
        self.cabal_copy(ext)

    def _cabal_component_name(self, ext: Extension):
        return ext.name.split(".")[-1]

    def _cabal_component_library_name(self, ext: Extension):
        return (
            "lib"
            + self._cabal_component_name(ext)
            + os.path.extsep
            + self._dynlib_extension()
        )

    def _cabal_component_library_path(self, ext: Extension):
        return os.path.join(self.build_temp, self._cabal_component_library_name(ext))

    def _dynlib_extension(self):
        if sys.platform == "darwin":
            return "dylib"
        if sys.platform == "linux":
            return "so"
        if sys.platform == "win32":
            return "dll"
        if sys.platform == "cygwin":
            return "dll"
        raise PlatformError(
            f"don't know how to create distributions on platform {self.plat_name}"
        )


setup(ext_modules=ext_modules, cmdclass={"build_ext": build_hs_ext})
