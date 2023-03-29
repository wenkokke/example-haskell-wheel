{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (when)
import qualified Data.Map as Map
import Distribution.Compat.Directory (doesPathExist)
import Distribution.Compat.Prelude (isSpace, unless)
import Distribution.PackageDescription (Benchmark (..), BuildInfo (..), ComponentName (..), Executable (..), ForeignLib (..), Library (..), PackageDescription (..), TestSuite (..), componentNameString, unUnqualComponentName)
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (ComponentLocalBuildInfo (..), LocalBuildInfo (..), componentBuildDir, showComponentName)
import Distribution.Simple.Program (Program, ProgramDb, getDbProgramOutput, requireProgram, runDbProgram, simpleProgram)
import Distribution.Simple.Setup (BuildFlags (..), ConfigFlags (..), configPrograms, emptyConfigFlags, fromFlagOrDefault)
import Distribution.Simple.Utils (die', getDirectoryContentsRecursive, info)
import Distribution.Utils.String (trim)
import Distribution.Verbosity (Verbosity)
import qualified Distribution.Verbosity as Verbosity (normal)
import System.Directory (copyFile)
import System.FilePath ((<.>), (</>))
import System.Info (os)
import Text.Printf (printf)

pythonPackagePath :: FilePath
pythonPackagePath = "fib"

pythonNativeModuleName :: FilePath
pythonNativeModuleName = "_binding"

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms =
          [swigProgram, pythonProgram] <> hookedPrograms simpleUserHooks,
        -- Add the Python include path to --extra-include-dirs
        confHook = \(genericPackageDescription, hookedBuildInfo) configFlags -> do
          localBuildInfo <- confHook simpleUserHooks (genericPackageDescription, hookedBuildInfo) configFlags
          let verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          let LocalBuildInfo {localPkgDescr, withPrograms} = localBuildInfo
          pythonIncludeDir <- findPythonIncludeDir verbosity withPrograms
          return localBuildInfo {localPkgDescr = addExtraIncludeDirs [pythonIncludeDir] localPkgDescr},
        -- Generates the interface files with swig
        postConf = \args configFlags packageDescription localBuildInfo -> do
          let verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          let LocalBuildInfo {withPrograms} = localBuildInfo
          swigGenerateInterface verbosity withPrograms
          postConf simpleUserHooks args configFlags packageDescription localBuildInfo,
        -- Copies the generated library to the Python package
        buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
          let verbosity = fromFlagOrDefault Verbosity.normal (buildVerbosity buildFlags)
          buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
          copyForeignLib verbosity localBuildInfo
      }

copyForeignLib :: Verbosity -> LocalBuildInfo -> IO ()
copyForeignLib verbosity localBuildInfo = do
  (foreignLibName, foreignLibBuildDir) <- findForeignLibNameAndBuildDir verbosity localBuildInfo
  let sourceName = "lib" <> foreignLibName <.> sharedLibExtension
  let sourcePath = foreignLibBuildDir </> sourceName
  let targetPath = pythonPackagePath </> pythonNativeModuleName <.> pythonLibExtension
  sourcePathExists <- doesPathExist sourcePath
  unless sourcePathExists $
    die' verbosity $
      printf "Could not find compiled library '%s' in %s" sourceName foreignLibBuildDir
  copyFile sourcePath targetPath

pythonLibExtension :: FilePath
pythonLibExtension
  | System.Info.os == "mingw32" = "dll"
  | otherwise = "so"

sharedLibExtension :: FilePath
sharedLibExtension
  | System.Info.os == "mingw32" = "dll"
  | System.Info.os == "darwin" = "dylib"
  | otherwise = "so"

findForeignLibNameAndBuildDir :: Verbosity -> LocalBuildInfo -> IO (String, FilePath)
findForeignLibNameAndBuildDir verbosity localBuildInfo = do
  let LocalBuildInfo {componentNameMap} = localBuildInfo
  let foreignLibNames = filter isForeignLibName (Map.keys componentNameMap)
  when (length foreignLibNames /= 1) $
    die' verbosity "Could not find unique foreign libraries"
  let [CFLibName foreignLibName] = foreignLibNames
  let foreignLibNameString = unUnqualComponentName foreignLibName
  let componentLocalBuildInfos = componentNameMap Map.! CFLibName foreignLibName
  let foreignLibLocalBuildInfos = filter isForeignLibComponentLocalBuildInfo componentLocalBuildInfos
  when (length foreignLibLocalBuildInfos /= 1) $
    die' verbosity "Could not find unique foreign libraries component"
  let [foreignLibLocalBuildInfo] = foreignLibLocalBuildInfos
  return (foreignLibNameString, componentBuildDir localBuildInfo foreignLibLocalBuildInfo)
  where
    isForeignLibName :: ComponentName -> Bool
    isForeignLibName CFLibName {} = True
    isForeignLibName _ = False

    isForeignLibComponentLocalBuildInfo :: ComponentLocalBuildInfo -> Bool
    isForeignLibComponentLocalBuildInfo FLibComponentLocalBuildInfo {} = True
    isForeignLibComponentLocalBuildInfo _ = False

swigProgram :: Program
swigProgram = simpleProgram "swig"

swigGenerateInterface :: Verbosity -> ProgramDb -> IO ()
swigGenerateInterface verbosity programDb = do
  runDbProgram
    verbosity
    swigProgram
    programDb
    [ "-python",
      "-o",
      "cbits/binding_wrap.c",
      "-outdir",
      pythonPackagePath,
      "cbits/binding.i"
    ]

pythonProgram :: Program
pythonProgram = simpleProgram "python"

findPythonIncludeDir :: Verbosity -> ProgramDb -> IO FilePath
findPythonIncludeDir verbosity programDb = do
  pythonOutput <- getDbProgramOutput verbosity pythonProgram programDb ["-c", script]
  let pythonIncludeDir = trim pythonOutput
  pythonIncludeDirExists <- doesPathExist pythonIncludeDir
  if pythonIncludeDirExists
    then return pythonIncludeDir
    else die' verbosity $ "Could not determine Python include directory. Found: " <> pythonIncludeDir
  where
    script :: String
    script =
      unlines
        [ "import sys;",
          "import platform;",
          "major, minor, _ = platform.python_version_tuple();",
          "print('%s/include/python%s.%s' % (sys.exec_prefix, major, minor));"
        ]

addExtraIncludeDirs :: [FilePath] -> PackageDescription -> PackageDescription
addExtraIncludeDirs extraIncludeDirs packageDescription =
  let extraBuildInfo = mempty {includeDirs = extraIncludeDirs}
      modifyLib l = l {libBuildInfo = libBuildInfo l <> extraBuildInfo}
      modifyExecutable e = e {buildInfo = buildInfo e <> extraBuildInfo}
      modifyForeignLib f = f {foreignLibBuildInfo = foreignLibBuildInfo f <> extraBuildInfo}
      modifyTestsuite t = t {testBuildInfo = testBuildInfo t <> extraBuildInfo}
      modifyBenchmark b = b {benchmarkBuildInfo = benchmarkBuildInfo b <> extraBuildInfo}
   in packageDescription
        { library = modifyLib `fmap` library packageDescription,
          subLibraries = modifyLib `map` subLibraries packageDescription,
          executables = modifyExecutable `map` executables packageDescription,
          foreignLibs = modifyForeignLib `map` foreignLibs packageDescription,
          testSuites = modifyTestsuite `map` testSuites packageDescription,
          benchmarks = modifyBenchmark `map` benchmarks packageDescription
        }
