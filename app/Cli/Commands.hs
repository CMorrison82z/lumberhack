module Cli.Commands where

import System.IO (readFile')
import System.Posix.Files
import System.Posix.Types (CIno)
import System.Directory
import qualified System.OsString as OSS
import qualified System.OsPath as OSP
import System.Console.Terminal.Size (Window (..), size)
import Control.Monad (unless, filterM, liftM2)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Bool
import Data.Bits
import Data.Foldable (traverse_)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Either (rights, partitionEithers)
import Data.List (find, union, isPrefixOf, intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as LNE
import System.FilePath (takeFileName, (</>))
import Data.Functor ((<&>))
import String.ANSI
import Logging
import Util
import Core

data OutputStrategy = IntoStdOut | IntoTmpDir

cliTagFile fp tags = do
    app <- makeApp

    userTagFile app fp tags >>= writeApp

cliUntagFile fp tags = do
    app <- makeApp

    userUntagFile app fp tags >>= writeApp

cliQuery tags qStrategy oStrategy = do
    app <- makeApp

    let userOutput IntoStdOut files = putStr $ unlines files
        userOutput IntoTmpDir files = do
            tmpDir <- getAppTmpDir
            traverse_ (\f -> createFileLink f $ tmpDir </> (takeFileName f)) files
            putStrLn tmpDir
    
    userQuery app qStrategy tags >>= traverse inodeStoreFilePath >>= userOutput oStrategy

cliViewTags fp = do
    app <- makeApp
    maybeTags <- userGetFileTags app . fileID <$> getFileStatus fp

    putStrLn $ maybe ("File `" ++ fp ++ "` has not been tagged") (renderTags . fmap T.unpack) maybeTags
   
cliQueryNames tags qStrategy includeLumberhackINodes = do
    app <- makeApp
    Window {width = window_width} <- fromMaybe (Window {width = 80, height = 24}) <$> size

    let fs = fmap (\xs -> (fst $ LNE.head xs, LNE.toList $ fmap snd xs))
            . LNE.groupAllWith fst 
            . filter (\(inode, filepath) -> includeLumberhackINodes || (not $ OSS.unsafeEncodeUtf (show inode) `OSS.isInfixOf` filepath))

    -- TODO:
    -- - Group by same inodes
    -- - filter out the name in the `lumberhack/store`
    -- - Show all tags each has, BOLD on the tags that matched the queried tags
    -- - format nicely
    userQuery app qStrategy tags 
        >>= userGetINodeNamesPairs >>= traverse (traverse OSP.decodeUtf . snd) . fs 
        >>= putStr . unlines . intersperse (terminalSeparator window_width) . fmap unlines

cliShowAll includeLumberhackINodes = do
    app <- makeApp
    Window {width = window_width} <- fromMaybe (Window {width = 80, height = 24}) <$> size
    
    allThings <- fmap (\t@(inode, _, fpaths) -> fmap (filter (\filepath -> includeLumberhackINodes || (not $ OSS.unsafeEncodeUtf (show inode) `OSS.isInfixOf` filepath))) t) <$> userShowApp app

    traverse (\(_, tags, fpaths) -> mappend (renderTags (fmap T.unpack tags) ++ "\n") . unlines <$> traverse OSP.decodeUtf fpaths) allThings
        >>= putStr . unlines . intersperse (terminalSeparator window_width)

cliListTags = makeApp >>= putStrLn . renderTags . fmap T.unpack . V.toList . userTags

cliGetZombies = putStrLn ("Zombie files :") >> getAppZombieFiles >>= putStrLn . unlines

cliDeleteZombies = makeApp >>= userRemoveAppZombieFiles >>= writeApp

cliRmTmpDirs = do
    tmpDir <- getTemporaryDirectory
    tmpDirFiles <- fmap (tmpDir </>) <$> listDirectory tmpDir
    appTDB <- appTmpDirBase
    traverse_ removeDirectoryRecursive . filter (isPrefixOf appTDB) $ tmpDirFiles
 
renderTags :: [String] -> String
renderTags ts = unwords . fmap applyFstToSnd $ zip (cycle [blue, brightRed]) ts

terminalSeparator window_width = replicate window_width '-'
