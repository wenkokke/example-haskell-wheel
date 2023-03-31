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
        self.mkpath(self.build_temp)
        self.cabal_configure(ext)
        self.cabal_build(ext)
        self.cabal_copy(ext)

    def cabal_configure(self, ext):
        args = ["configure"]
        args.extend(f"--extra-lib-dirs={dir}" for dir in self.library_dirs)
        args.extend(f"--extra-include-dirs={dir}" for dir in self.include_dirs)
        args.append(f"--extra-prog-path={os.path.dirname(sys.executable)}")
        args.extend(
            [
                f"--prefix={os.path.abspath(self.build_temp)}",
                f"--libdir=",
                f"--dynlibdir=",
                f"--datadir=",
                f"--docdir=",
                f"--builddir={os.path.abspath(self.build_temp)}",
            ]
        )
        self.cabal(args)

    def cabal_build(self, ext):
        args = ["build"]
        args.append(f"--builddir={self.build_temp}")
        self.cabal(args)

    def cabal_copy(self, ext):
        args = ["copy"]
        args.append(f"--builddir={self.build_temp}")
        args.append(f"--destdir={self.build_temp}")
        self.cabal(args)
        ext_source = self.cabal_component_library_path(ext)
        ext_target = self.get_ext_fullpath(ext.name)
        self.mkpath(os.path.dirname(ext_target))
        self.copy_file(ext_source, ext_target)

    def cabal(self, args):
        args = [self.find_runhaskell(), "Setup.hs", *args]
        cmd = " ".join(args)
        print(cmd)
        exitCode = subprocess.call(args)
        if exitCode != 0:
            raise ExecError(f"error occurred when running '{cmd}'")

    def cabal_component_library_path(self, ext):
        return os.path.join(self.build_temp, self.cabal_component_library_name(ext))

    def cabal_component_library_name(self, ext):
        dynlib_extension = {
            "darwin": "dylib",
            "linux": "so",
            "win32": "dll",
            "cygwin": "dll",
        }.get(sys.platform, None)
        if not dynlib_extension:
            raise PlatformError(f"unsupported platform {self.plat_name}")
        return f"lib{self.cabal_component_name(ext)}{os.path.extsep}{dynlib_extension}"

    def cabal_component_name(self, ext):
        return ext.name.split(".")[-1]

    _runhaskell: Optional[str] = None

    def find_runhaskell(self):
        if self._runhaskell is None:
            self._runhaskell = find_executable("runhaskell")
            if self._runhaskell is None:
                raise ExecError("could not find executable 'runhaskell'")
        return self._runhaskell


setup(ext_modules=ext_modules, cmdclass={"build_ext": build_hs_ext})
