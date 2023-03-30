{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription (ComponentName (..), ForeignLib (..), PackageDescription (..), showComponentName, unUnqualComponentName)
import Distribution.Pretty (Pretty (..))
import Distribution.Simple (PackageIdentifier (..), UserHooks (..), defaultMainWithHooks, simpleUserHooks, versionNumbers)
import Distribution.Simple.LocalBuildInfo (ComponentLocalBuildInfo, LocalBuildInfo (..), componentBuildDir)
import Distribution.Simple.Program (ConfiguredProgram (..), Program, ProgramDb, requireProgram, runDbProgram, runProgram, simpleProgram)
import Distribution.Simple.Setup (BuildFlags (..), fromFlagOrDefault)
import Distribution.Simple.Utils (die', findFirstFile)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (getDirectoryContents)
import System.FilePath ((<.>), (</>))
import System.Info (os)
import Text.PrettyPrint (render)

type PythonPackageName = FilePath

type HaskellLibraryName = String

pythonProgram :: Program
pythonProgram = simpleProgram "python"

installNameToolProgram :: Program
installNameToolProgram = simpleProgram "install_name_tool"

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = [pythonProgram, installNameToolProgram],
        -- Generates a Python package
        postBuild = \args buildFlags packageDescription localBuildInfo -> do
          let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
          (foreignLibName, foreignLibDir) <- findForeignLibNameAndBuildDir verbosity packageDescription localBuildInfo
          let pythonPackageName = toPythonPackageName foreignLibName

          -- Create pyproject.toml
          let PackageDescription {package, author, maintainer, description, licenseRaw} = packageDescription
          let PackageIdentifier {pkgVersion} = package
          let license = either (render . pretty) (render . pretty) licenseRaw
          let version = intercalate "." (map show (versionNumbers pkgVersion))
          writeFile "pyproject.toml" (pyprojectTomlTemplate pythonPackageName version (fromShortText author) (fromShortText maintainer) (fromShortText description) license)

          -- Create build.py
          let LocalBuildInfo {withPrograms} = localBuildInfo
          let python = runDbProgram verbosity pythonProgram withPrograms
          let pipx = python . (["-m", "pipx"] <>)
          writeFile "build.py" (buildPyTemplate pythonPackageName foreignLibName foreignLibDir)

          -- Create setup.py
          -- writeFile "setup.py" setupPyTemplate

          -- Build the extension:
          python ["build.py"]

          -- Build the wheel:
          pipx ["run", "--spec", "build", "pyproject-build", "--wheel"]

          -- Delocate the wheel:
          when (System.Info.os == "darwin") $
            writeFile "delocate.sh" (delocateShTemplate foreignLibDir)

          -- Check the wheel:
          pipx ["run", "twine", "check", "dist/*.whl"]
      }

pyprojectTomlTemplate pythonPackageName version authorName authorEmail description license =
  unlines
    [ "[build-system]",
      "requires = ['poetry-core>=1.5.0']",
      "build-backend = 'poetry.core.masonry.api'",
      "",
      "[tool.poetry]",
      "name = '" <> pythonPackageName <> "'",
      "version = '" <> version <> "'",
      "authors = ['" <> authorName <> " <" <> authorEmail <> ">']",
      "description = '" <> description <> "'",
      "license = '" <> license <> "'",
      "include = [",
      "  # Build script must be included in the sdist",
      "  { path = 'build.py', format = 'sdist' },",
      "  # C extensions must be included in the wheel",
      "  { path = '" <> pythonPackageName <> "/*.so', format = 'wheel' },",
      "  { path = '" <> pythonPackageName <> "/*.pyd', format = 'wheel' },",
      "]",
      "",
      "[tool.poetry.build]",
      "script = 'build.py'",
      "generate-setup-file = false"
    ]

delocateShTemplate foreignLibDir =
  unlines
    [
      "#!/bin/bash",
      "for whl in ./dist/*-macosx-*.whl; do",
      "  DYLD_LIBRARY_PATH="<>foreignLibDir<>" python -m pipx run --spec delocate delocate-wheel ${whl}",
      "done"
    ]

buildPyTemplate pythonPackageName foreignLibName foreignLibDir =
  unlines
    [ "from distutils.command.build_ext import build_ext",
      "from distutils.core import Distribution, Extension",
      "import os",
      "import platform",
      "import shutil",
      "",
      "ext_modules = [",
      "    Extension(",
      "        name='" <> pythonPackageName <> "._binding',",
      "        libraries=['" <> foreignLibName <> "'],",
      "        library_dirs=['" <> foreignLibDir <> "'],",
      "        sources=['./" <> pythonPackageName <> "/binding.i'],",
      "    )",
      "]",
      "",
      "",
      "def build():",
      "    distribution = Distribution({",
      "      'name': '" <> pythonPackageName <> "',",
      "      'ext_modules': ext_modules",
      "})",
      "    distribution.package_dir = '" <> pythonPackageName <> "'",
      "",
      "    cmd = build_ext(distribution)",
      "    cmd.ensure_finalized()",
      "    cmd.run()",
      "",
      "    # Copy built extensions back to the project",
      "    for output in cmd.get_outputs():",
      "        relative_extension = os.path.relpath(output, cmd.build_lib)",
      "        shutil.copyfile(output, relative_extension)",
      "        mode = os.stat(relative_extension).st_mode",
      "        mode |= (mode & 0o444) >> 2",
      "        os.chmod(relative_extension, mode)",
      "",
      "    # Workaround for issue with RPATH on macOS",
      "    # See: https://github.com/pypa/cibuildwheel/issues/816",
      "    if platform.system() == 'Darwin':",
      "        os.environ['DYLD_LIBRARY_PATH'] = '"<> foreignLibDir <>"'",
      "        delocate.delocate_path('"<> pythonPackageName <>"', '"<> pythonPackageName <>"/.dylibs')",
      "",
      "if __name__ == '__main__':",
      "    build()",
      ""
    ]

toPythonPackageName :: HaskellLibraryName -> PythonPackageName
toPythonPackageName foreignLibName = map toLower (fromMaybe foreignLibName (stripPrefix "HS" foreignLibName))

fixModuleRpathOnMacOS :: Verbosity -> ProgramDb -> PythonPackageName -> HaskellLibraryName -> FilePath -> IO ()
fixModuleRpathOnMacOS verbosity programDb pythonPackageName foreignLibName foreignLibDir = do
  let installNameTool = runDbProgram verbosity installNameToolProgram programDb
  let python = runDbProgram verbosity pythonProgram programDb
  let pipx = python . (["-m", "pipx"] <>)
  let foreignLibFullName = "lib" <> foreignLibName <.> "dylib"
  let isBindingLibName libName = "_binding" `isPrefixOf` libName && ".so" `isSuffixOf` libName
  bindingLibFullNames <- filter isBindingLibName <$> getDirectoryContents pythonPackageName
  when (length bindingLibFullNames /= 1) $
    die' verbosity $
      "Could not find unique Python extension library: " <> show bindingLibFullNames
  pipx ["run", "--spec", "delocate", "delocate-wheel", "dist/*.whl"]

findForeignLibNameAndBuildDir :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO (HaskellLibraryName, FilePath)
findForeignLibNameAndBuildDir verbosity packageDescription localBuildInfo = do
  let PackageDescription {foreignLibs} = packageDescription
  when (length foreignLibs /= 1) $
    die' verbosity "Could not find unique foreign library"
  let [ForeignLib {foreignLibName}] = foreignLibs
  let LocalBuildInfo {componentNameMap} = localBuildInfo
  let componentLocalBuildInfos = componentNameMap Map.! CFLibName foreignLibName
  when (length componentLocalBuildInfos /= 1) $
    die' verbosity "Could not find unique foreign libraries component"
  let [componentLocalBuildInfo] = componentLocalBuildInfos
  return (unUnqualComponentName foreignLibName, componentBuildDir localBuildInfo componentLocalBuildInfo)
