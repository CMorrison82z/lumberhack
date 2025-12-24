module Cli.Interface where

import Core
import Debug.Trace
import Cli.Commands
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Options.Applicative

cli :: Parser (IO ())
cli =
  subparser
    ( command "tag" ( info
            -- FIXME:
            -- I'd like to customize completions via `(metavar "FILE" <> (action "file"))`, but it seems to have no effect. (by default, it seems to use `file`, 
            -- but testing it with `"job"` instead does not complete to jobs).
            ( cliTagFile <$> strArgument (metavar "FILE")
            -- TODO: I'd like to support tagging multiple files at once (good with `lf $lmhk $fx foo bar`). This requires using something (probably an option) to disambiguate
            -- <*> option (maybeReader $ \x -> if all (isAlphaNum . traceShowId) x then Just (fmap T.pack $ words x) else Nothing) (long "tag" <> metavar "TAGS...")
            -- <*> option (some str) (long "tag" <> metavar "TAGS...")
            -- <*> some (strOption (long "tag" <> metavar "TAGS..."))
            <*> some (strArgument (metavar "TAGS..."))
            )
            (progDesc "Tag a file with the given tags")
        )
        <> command
          "untag"
          ( info
            ( cliUntagFile <$> strArgument (metavar "FILE")
              <*> some (strArgument (metavar "TAGS..."))
            )
            (progDesc "Untag file from selected tags")
          )
        <> command
          "get"
          ( info
              ( (cliQuery <$> some (strArgument (metavar "TAGS...")))
                  <*> flag QOr QAnd
                    ( long "and"
                        <> short 'n'
                        <> help "Switch search strategy to `and` mode. Get files whose Tags contain at least all provided Tags."
                    )
                  <*> flag IntoStdOut IntoTmpDir
                    ( long "tmp"
                        <> short 't'
                        <> help "Output the files (as symlinks) into a directory in `/tmp`."
                    )
              )
              (progDesc "Retrieve files that have been tagged with, by default, at least one of the provided tags. Other query strategies are available (see `--help`")
          )
        <> command
          "get_names"
          ( info
              ( (cliQueryNames <$> some (strArgument (metavar "TAGS...")))
                  <*> flag QOr QAnd
                    ( long "and"
                        <> short 'n'
                        <> help "Get files whose Tags contain at least all provided Tags. Switch search strategy to `and` mode."
                    )
                  <*> switch
                    ( long "include-app"
                        <> help "Include names of links that are created and managed by lumberhack."
                    )
                <**> helper
              )
              (progDesc "Retrieve all the names files that have been tagged with, by default, at least one of the provided tags. Other query strategies are available (see `--help`)"
                        <> (footer $ "By default, this only checks names under the user's home directory (XdgHome)."
                            ++ "\nNOTE: This is relatively slow because it requires traversing the entire filesystem."))
          )
        <> command
          "view_tags"
          ( info (cliViewTags <$> strArgument (metavar "FILE"))
                (progDesc "Show all the tags the file has been assigned.")
          )
        <> command
          "show"
          ( info 
                (pure cliShowAll
                  <*> switch
                    ( long "include-app"
                        <> help "Include names of links that are created and managed by lumberhack."
                    )
                )
                (progDesc "Show the entire state of the app (all names of files that have been tagged, and the tags they've been assigned).")
          )
        <> command
          "tags"
          ( info
                (pure cliListTags)
                (progDesc "Display all the tags that exist on files")
          )
        <> command
          "view_zombies"
          ( info
                (pure cliGetZombies)
                (progDesc "Display files that do not exist anywhere except in the app's store.")
          )
        <> command
          "rm_zombies"
          ( info
                (pure cliDeleteZombies)
                (progDesc "Delete all zombie files (files that do not exist on the file system, except within the app's store.")
          )
        <> command
          "rm_tmp"
          ( info
                (pure cliRmTmpDirs)
                (progDesc "Delete all temporary directories that were created from `get --tmp`")
          )
        -- NOTE: Since modifying the data files directly can break the app quite easily, providing this isn't necessary (or a good idea).
        -- <> command
        --   "path"
        --   ( info
        --       (pure $ defaultJournalFile >>= putStrLn)
        --       (progDesc "Output path to data file.")
        --   )
    )


-- readPWord = Tag <$> (skipSpaces >> char '@' >> munch1 isAlphaNum)
--
-- readPWord = munch1 isAlphaNum
-- readPWords = sepBy (readPWord) (char ' ')
-- -- readPWords = sepBy1 (readPWord) (skipSingleSpaces)
--
-- skipSingleSpaces :: ReadP ()
-- skipSingleSpaces = look >>= skip
--  where
--   skip (c:s) | c == ' ' = get >> skip s
--   skip _                 = return ()
