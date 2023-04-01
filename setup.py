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
        self.cabal_configure()
        self.cabal_build(ext)

    def cabal_configure(self):
        self.cabal(
            [
                "configure",
                *(f"--extra-lib-dirs={dir}" for dir in self.library_dirs),
                *(f"--extra-include-dirs={dir}" for dir in self.include_dirs),
                *(f"--ghc-options=-optl-l{library}" for library in self.libraries),
            ]
        )

    def cabal_build(self, ext):
        self.mkpath(self.build_temp)
        self.cabal(["build"], env={"INSTALLDIR": self.build_temp, **os.environ})
        lib_filename = self.get_cabal_foreign_library_filename(ext)
        ext_fullpath = self.get_ext_fullpath(ext.name)
        self.mkpath(os.path.dirname(ext_fullpath))
        self.copy_file(os.path.join(self.build_temp, lib_filename), ext_fullpath)

    def get_cabal_foreign_library_filename(self, ext):
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

    def cabal(self, args, *, env=None):
        args = [self.find_cabal(), *args]
        cmd = " ".join(args)
        print(cmd)
        exitCode = subprocess.call(args, env=env)
        if exitCode != 0:
            raise DistutilsExecError(f"error occurred when running '{cmd}'")

    _cabal: Optional[str] = None

    def find_cabal(self):
        if self._cabal is None:
            self._cabal = find_executable("cabal")
            if self._cabal is None:
                raise DistutilsExecError("could not find executable 'cabal'")
        return self._cabal


setup(ext_modules=ext_modules, cmdclass={"build_ext": build_hs_ext})
