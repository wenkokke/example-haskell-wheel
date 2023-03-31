{-# LANGUAGE NamedFieldPuns #-}

import Distribution.Make (defaultMain)
import Distribution.Simple (UserHooks(..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup (ConfigFlags(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))

main :: IO ()
main = 
  defaultMainWithHooks
    simpleUserHooks
