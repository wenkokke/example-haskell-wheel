{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription (ComponentName (..), ForeignLib (..), PackageDescription (..), showComponentName, unPackageName, unUnqualComponentName)
import Distribution.Pretty (Pretty (..))
import Distribution.Simple (PackageIdentifier (..), UserHooks (..), defaultMainWithHooks, simpleUserHooks, versionNumbers)
import Distribution.Simple.LocalBuildInfo (ComponentLocalBuildInfo, LocalBuildInfo (..), componentBuildDir)
import Distribution.Simple.Program (ConfiguredProgram (..), Program, ProgramDb, getDbProgramOutput, needProgram, requireProgram, runDbProgram, runProgram, simpleProgram)
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

pipxProgram :: Program
pipxProgram = simpleProgram "pipx"

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = [pythonProgram, pipxProgram],
        -- Generates a Python package
        postBuild = \args buildFlags packageDescription localBuildInfo -> do
          let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)

          -- Get package information
          let PackageDescription {package, author, maintainer, description, licenseRaw} = packageDescription
          let PackageIdentifier {pkgName, pkgVersion} = package
          let package = unPackageName pkgName
          let license = render $ either pretty pretty licenseRaw
          let version = intercalate "." (map show (versionNumbers pkgVersion))

          -- Get build information
          (library, libraryDir) <- findForeignLibInfo verbosity packageDescription localBuildInfo
          let LocalBuildInfo {withPrograms} = localBuildInfo

          -- Create pyproject.toml, build.py, and paths.py:
          writeFile "pyproject.toml" $
            pyprojectTomlTemplate
              (escape package)
              (escape version)
              (escape $ fromShortText author)
              (escape $ fromShortText maintainer)
              (escape $ fromShortText description)
              (escape license)
          writeFile "build.py" $
            buildPyTemplate (escape package) (escape library)
          writeFile "paths.py" $
            pathsPyTemplate (escape libraryDir)

          -- Build the wheel:
          pipx verbosity withPrograms ["run", "--spec", "build", "pyproject-build", "--wheel"]
          -- Check the wheel:
          pipx verbosity withPrograms ["run", "twine", "check", "dist/*.whl"]
      }

python :: Verbosity -> ProgramDb -> [String] -> IO ()
python verbosity = runDbProgram verbosity pythonProgram

pipx :: Verbosity -> ProgramDb -> [String] -> IO ()
pipx verbosity programDb args = do
  maybePipxProgram <- needProgram verbosity pipxProgram programDb
  case maybePipxProgram of
    Nothing -> do
      pipxExists <- doesPipxExist verbosity programDb
      unless pipxExists . die' verbosity . unlines $
        [ "",
          "The program 'pipx' is required but it could not be found.",
          "See: https://pypa.github.io/pipx/#install-pipx"
        ]
      runDbProgram verbosity pythonProgram programDb ("-m" : "pipx" : args)
    Just (pipxProgram, programDb) -> do
      runProgram verbosity pipxProgram args

doesPipxExist :: Verbosity -> ProgramDb -> IO Bool
doesPipxExist verbosity programDb = do
  output <-
    getDbProgramOutput
      verbosity
      pythonProgram
      programDb
      ["-c", "import importlib.util; print(importlib.util.find_spec('pipx') is not None)"]
  return $ output == "True"

pyprojectTomlTemplate :: String -> String -> String -> String -> String -> String -> String
pyprojectTomlTemplate packageName version authorName authorEmail description license =
  unlines
    [ "[build-system]",
      "requires = ['poetry-core>=1.5.0', 'delocate; platform_system==\"Darwin\"']",
      "build-backend = 'poetry.core.masonry.api'",
      "",
      "[tool.poetry]",
      "name = '" <> packageName <> "'",
      "version = '" <> version <> "'",
      "authors = ['" <> authorName <> " <" <> authorEmail <> ">']",
      "description = '" <> description <> "'",
      "readme = 'README.md'",
      "license = '" <> license <> "'",
      "include = [",
      "  # Build script must be included in the sdist",
      "  { path = 'build.py', format = 'sdist' },",
      "  { path = 'paths.py', format = 'sdist' },",
      "  # C extensions must be included in the wheel",
      "  { path = '" <> packageName <> "/*.so', format = 'wheel' },",
      "  { path = '" <> packageName <> "/*.dylib', format = 'wheel' },",
      "  { path = '" <> packageName <> "/*.pyd', format = 'wheel' },",
      "]",
      "",
      "[tool.poetry.build]",
      "script = 'build.py'",
      "generate-setup-file = false"
    ]

pathsPyTemplate :: String -> String
pathsPyTemplate libraryDir =
  unlines
    [ "extra_library_dirs = ['" <> libraryDir <> "']"
    ]

buildPyTemplate :: String -> String -> String
buildPyTemplate package library =
  unlines
    [ "from distutils.command.build_ext import build_ext",
      "from distutils.core import Distribution, Extension",
      "import os",
      "import platform",
      "import paths",
      "import shutil",
      "",
      "for extra_library_dir in paths.extra_library_dirs:",
      "  print(os.listdir(extra_library_dir))",
      "",
      "ext_modules = [",
      "    Extension(",
      "        name='" <> package <> "._binding',",
      "        libraries=['" <> library <> "'],",
      "        library_dirs=paths.extra_library_dirs,",
      "        sources=['./" <> package <> "/binding.i'],",
      "    )",
      "]",
      "",
      "",
      "def build():",
      "    distribution = Distribution({",
      "      'name': '" <> package <> "',",
      "      'ext_modules': ext_modules",
      "})",
      "    distribution.package_dir = '" <> package <> "'",
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
      "        os.environ['DYLD_LIBRARY_PATH'] = os.pathsep.join(paths.extra_library_dirs)",
      "        import delocate",
      "        delocate.delocate_path('" <> package <> "', '" <> package <> "')",
      "",
      "if __name__ == '__main__':",
      "    build()",
      ""
    ]

findForeignLibInfo :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO (HaskellLibraryName, FilePath)
findForeignLibInfo verbosity packageDescription localBuildInfo = do
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

-- | Escape a string to print as a Python single-quote string.
escape :: String -> String
escape [] = []
escape ('\\' : cs) = '\\' : '\\' : escape cs
escape ('\'' : cs) = '\\' : '\'' : escape cs
escape (char : cs) = char : escape cs
