module Main (main) where

import Core
import Cli.Interface (cli)
import Control.Monad (join)
import Options.Applicative

import Cli.Commands

main :: IO ()
main = initAppDirectory >> (join $ customExecParser p opts)
  where
    opts =
      info
        (cli <**> helper)
        ( fullDesc
            <> progDesc appName
            <> header
              ( appName
                  ++ " - "
                  ++ "Interface the filesystem as Sets rather than Trees"
              )
        )
    p = prefs showHelpOnEmpty
