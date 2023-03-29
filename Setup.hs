{-# LANGUAGE NamedFieldPuns #-}

import Distribution.Compat.Directory (doesPathExist)
import Distribution.Compat.Prelude (isSpace)
import Distribution.PackageDescription (Benchmark (..), BuildInfo (..), Executable (..), ForeignLib (..), Library (..), PackageDescription (..), TestSuite (..))
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Program (Program, ProgramDb, getDbProgramOutput, requireProgram, simpleProgram, runDbProgram)
import Distribution.Simple.Setup (BuildFlags (..), ConfigFlags (..), configPrograms, emptyConfigFlags, fromFlagOrDefault)
import Distribution.Simple.Utils (die', info)
import Distribution.Utils.String (trim)
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity as Verbosity (normal)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms =
          [swigProgram, pythonProgram] <> hookedPrograms simpleUserHooks,
        confHook = \(genericPackageDescription, hookedBuildInfo) configFlags -> do
          localBuildInfo <- confHook simpleUserHooks (genericPackageDescription, hookedBuildInfo) configFlags
          let verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          let LocalBuildInfo {localPkgDescr, withPrograms} = localBuildInfo
          pythonIncludeDir <- findPythonIncludeDir verbosity withPrograms
          return localBuildInfo { localPkgDescr = addExtraIncludeDirs [pythonIncludeDir] localPkgDescr },
        postConf = \args configFlags packageDescription localBuildInfo -> do
          let verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          let LocalBuildInfo {withPrograms} = localBuildInfo
          swigGenerateInterface verbosity withPrograms
          postConf simpleUserHooks args configFlags packageDescription localBuildInfo
      }

swigProgram :: Program
swigProgram = simpleProgram "swig"

swigGenerateInterface :: Verbosity -> ProgramDb -> IO ()
swigGenerateInterface verbosity programDb = do
  runDbProgram verbosity swigProgram programDb [
      "-python",
      "-o", "cbits/binding_wrap.c",
      "-outdir", "fib",
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
