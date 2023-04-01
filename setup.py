import os.path
import sys
import subprocess
from typing import Optional
from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext
from distutils.errors import (
    DistutilsExecError,
    DistutilsPlatformError,
    DistutilsSetupError,
)
from distutils.spawn import find_executable

ext_modules = [
    Extension(
        name="example_haskell_wheel._binding",
        sources=["src/example_haskell_wheel/binding.i"],
    ),
]


class build_hs_ext(build_ext):
    def finalize_options(self):
        super().finalize_options()

        if sys.platform in ["win32", "cygwin"]:
            self.libraries.append("python%d%d" % sys.version_info[:2])

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
        self.cabal(
            [
                "configure",
                f"--prefix={os.path.abspath(self.build_temp)}",
                f"--libdir=",
                f"--dynlibdir=",
                f"--datadir=",
                f"--docdir=",
                f"--builddir={self.build_temp}",
                f"--extra-prog-path={os.path.dirname(sys.executable)}",
                *(f"--extra-lib-dirs={dir}" for dir in self.library_dirs),
                *(f"--extra-include-dirs={dir}" for dir in self.include_dirs),
                *(f"--ghc-options=-optl-l{library}" for library in self.libraries),
            ]
        )

    def cabal_build(self, ext):
        self.cabal(["build", f"--builddir={self.build_temp}"])

    def cabal_copy(self, ext):
        self.cabal(
            [
                "copy",
                f"--builddir={self.build_temp}",
                f"--destdir={self.build_temp}",
            ]
        )
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
            raise DistutilsExecError(f"error occurred when running '{cmd}'")

    def cabal_component_library_path(self, ext):
        return os.path.join(self.build_temp, self.cabal_component_library_name(ext))

    def cabal_component_library_name(self, ext):
        if sys.platform not in ["darwin", "linux", "win32", "cygwin"]:
            raise DistutilsPlatformError(f"unsupported platform {self.plat_name}")
        library_prefix = "" if sys.platform in ["win32", "cygwin"] else "lib"
        component_name = ext.name.split(".")[-1]
        dynlib_extension = {
            "darwin": "dylib",
            "linux": "so",
            "win32": "dll",
            "cygwin": "dll",
        }[sys.platform]
        return f"{library_prefix}{component_name}{os.path.extsep}{dynlib_extension}"

    _runhaskell: Optional[str] = None

    def find_runhaskell(self):
        if self._runhaskell is None:
            self._runhaskell = find_executable("runhaskell")
            if self._runhaskell is None:
                raise DistutilsExecError("could not find executable 'runhaskell'")
        return self._runhaskell


setup(ext_modules=ext_modules, cmdclass={"build_ext": build_hs_ext})
