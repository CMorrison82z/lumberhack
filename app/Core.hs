module Core where

import Debug.Trace
import System.IO (readFile')
import System.Posix.Files
import System.Posix.Types (CIno(..))
import qualified System.Posix.Files.PosixString as FilesPosStr
import System.Directory
import System.OsPath (OsPath)
import qualified System.OsPath as OSP
import System.OsString.Internal.Types (OsString (getOsString), PosixString (getPosixString))
import Control.Monad (unless, filterM, liftM2, (<=<))
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString  as S8
import Data.Serialize
import GHC.Generics
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set
import Data.Bool
import Data.Bits
import Data.Foldable (traverse_)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Either (fromRight, rights, partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as LNE
import Data.List (find, union, isPrefixOf)
import System.FilePath (takeFileName, (</>))
import Data.Functor ((<&>))
import Logging
import Util

type TaggedInodes = Map CIno Integer

data App = App {
        taggedInodes :: TaggedInodes,
        userTags :: Vector Text
    }
    deriving (Show, Generic)

data QStrategy = QOr | QAnd

instance Serialize Text where
  put t = put (T.unpack t)
  get = T.pack <$> get

instance Serialize a => Serialize (Vector a) where
  put xs = put (V.toList xs)
  get = V.fromList <$> get

instance Serialize CIno where
  put (CIno w64) = put w64
  get = CIno <$> get

instance Serialize App

userQuery App {taggedInodes, userTags} qStrategy tags = do
    let stratF = case qStrategy of
            QOr -> any
            QAnd -> all
        (dneFlags, flags) = partitionEithers $ tagsFlags userTags tags

    unless (length dneFlags == 0) $ noticeM "user query" $ "Non-existent flags " ++ (filter ('"' /=) $ show dneFlags)
    
    pure . fmap fst
        . filter (\(_, setFlags) -> stratF (\qFlag -> (setFlags .&. qFlag) == qFlag) flags)
        $ M.assocs taggedInodes

userTagFile :: App -> FilePath -> [Text] -> IO App
userTagFile app fp tags = do
    fstat <- getFileStatus fp

    storeFile fp

    pure $ tagFile app fstat tags

userUntagFile :: App -> FilePath -> [Text] -> IO App
userUntagFile app fp tags = do
    fstat <- getFileStatus fp

    pure $ untagFile app fstat tags

storeFile f = do
    fstat <- getFileStatus f
    storeDir <- storeDataDirectory

    directoryHasFile storeDir fstat >>= (flip unless $ newStoreFilePath fstat >>= createLink f)

userGetFileTags App {taggedInodes, userTags} inode = fmap (userTags V.!) . enabledFlags <$> taggedInodes M.!? inode

userRemoveAppZombieFiles :: App -> IO App
userRemoveAppZombieFiles (app@App {taggedInodes}) = do
    zombies <- getAppZombieFiles
    zombieInodes <- traverse (fmap fileID . getFileStatus) zombies
    
    traverse_ removeFile zombies

    pure $ app {
            -- taggedInodes = foldr (\zf taggedInodes' ->  (fileID $ getFileStatus zf) `M.delete` taggedInodes') taggedInodes zombies
            taggedInodes = taggedInodes `M.withoutKeys` (Data.Set.fromList zombieInodes)
        }

getAppZombieFiles :: IO [FilePath]
getAppZombieFiles = do
    storeDir <- storeDataDirectory
    dirFiles <- (fmap . fmap) (storeDir </>) $ listDirectory storeDir

    filterM (fmap isZombieFile . getFileStatus) dirFiles 

-- NOTE: Get names for a list of inodes rather than individually because traversing the filesystem is extremely expensive
userGetINodeNamesPairs :: [CIno] -> IO [(CIno, OsPath)]
userGetINodeNamesPairs inodes = do
    homeFiles <- getHomeDirectory >>= OSP.encodeUtf >>= getFilesRecursiveAbs
    -- PERF: `filterMLazy` has to happen first to preserve a lazy stream.
    traverse (\p -> fmap ((, p) . fileID) . FilesPosStr.getFileStatus $ getOsString p) =<< filterMLazy (fmap (((flip elem) inodes) . fileID) . FilesPosStr.getFileStatus . getOsString) homeFiles

userShowApp :: App -> IO [(CIno, [Text], [OsPath])]
userShowApp (app@App {taggedInodes, userTags}) = do
    let groupToInodes = fmap (\xs -> (fst $ LNE.head xs, LNE.toList $ fmap snd xs))
            . LNE.groupAllWith fst 

    fs <- userGetINodeNamesPairs $ M.keys taggedInodes
    
    pure $ fmap (\(inode, fpaths) -> (inode, fromMaybe [] $ userGetFileTags app inode, fpaths)) $ groupToInodes fs
    
-- NOTE:
-- Slight inefficiency here where `tagsFlags` already indicates missing members. This could be used to
-- simply append new tags to `userTags`. However, it's a bit messy because those `Left` have got to again
-- be converted into their associated bitFlag afterwards.
-- TODO:
-- Implement `tag` validation (isAlphaNum || c == ' ' || c == ',')
tagFile App {taggedInodes, userTags} fstat tags = let 
        inode = fileID fstat
        newUserTags = updateTags userTags tags
        tagsBOred = foldr (.|.) 0 . rights .  tagsFlags newUserTags $ tags
        alterFlags m = Just $ fromMaybe 0 m .|. tagsBOred
    in App {taggedInodes = M.alter alterFlags inode taggedInodes, userTags = newUserTags}

untagFile (app@App {taggedInodes, userTags}) fstat tags = let 
        inode = fileID fstat
        tagsBOred = foldr (.|.) 0 . rights .  tagsFlags userTags $ tags
        -- `adjust` does NOT add files that don't exist (otherwise we might add new files to the store that had no tags)
        adjustFlags m = m .&. (complement tagsBOred)
    in app {taggedInodes = M.adjust adjustFlags inode taggedInodes}

updateTags :: Vector Text -> [Text] -> Vector Text
updateTags userTags tags = V.fromList $ union (V.toList userTags) tags

tagFlag :: Vector Text -> Text -> Maybe Integer
tagFlag userTags tag = bit <$> tag `V.elemIndex` userTags

tagsFlags :: Vector Text -> [Text] -> [Either Text Integer]
tagsFlags userTags = fmap (\tag -> fromMaybe (Left tag) $ Right <$> tagFlag userTags tag)

inodeStoreFileName inode = "inode_" <> (show inode)

inodeStoreFilePath :: CIno -> IO FilePath
inodeStoreFilePath inode = storeDataDirectory <&> (</> inodeStoreFileName inode)

newStoreFilePath :: FileStatus -> IO FilePath
newStoreFilePath = inodeStoreFilePath . fileID
    
directoryHasFile dir fstat = do
    dirFiles <- fmap (dir </>) <$> listDirectory dir
    any (\x -> fileID fstat == fileID x) <$> traverse getFileStatus dirFiles 

filterZombieFiles = filterM (fmap isZombieFile . getFileStatus)

isZombieFile fstat = linkCount fstat == 1

enabledFlags = bitIndices

bitIndices :: Integer -> [Int]
bitIndices x = take (popCount x) . filter (testBit x) $ [0..]

appTmpDirBase = (</> appName ++ "_output") <$> getTemporaryDirectory 

getAppTmpDir = do
    baseName <- appTmpDirBase
    newDir <- fmap head . dropWhileM doesDirectoryExist . fmap (mappend baseName . show) $ [0..]
    createDirectory newDir
    return newDir

makeApp :: IO App
makeApp = appDataFile >>= fmap (fromRight (error "Failed to decode application data") . decode) . S8.readFile

writeApp :: App -> IO ()
writeApp app = appDataFile >>= (flip S8.writeFile $ encode app)

-- NOTE: Old Ser/De methods
-- makeApp = App 
--     <$> (taggedInodesDataFile >>= fmap parseTaggedInodes . readFile')
--     <*> (tagsDataFile >>= fmap parseTagsList . readFile')
-- writeApp App {taggedInodes, userTags} =
--     taggedInodesDataFile >>= (flip writeFile $ serializeTaggedInodes taggedInodes)
--     >> tagsDataFile >>= (flip writeFile $ serializeTagsList userTags)
-- parseTaggedInodes :: String -> TaggedInodes
-- parseTaggedInodes = M.fromList . fmap read . tail . lines
--
-- serializeTaggedInodes :: TaggedInodes -> String
-- serializeTaggedInodes = unlines . (fileWarningMessage :) . fmap show . M.toList
--
-- parseTagsList :: String -> Vector Text
-- parseTagsList s = V.fromList . fmap T.pack . quotedWords $ fromMaybe "" $ lines s !? 1
--
-- serializeTagsList :: Vector Text -> String
-- serializeTagsList = (fileWarningMessage ++) . ('\n' :) . wordsToCsv . fmap T.unpack . V.toList

fileWarningMessage = "! CORRECTNESS OF THIS APPLICATION HIGHLY DEPENDS ON THE STATE OF THIS FILE. MODIFYING IT DIRECTLY IS LIKELY TO BREAK THE APPLICATION !"

appName :: FilePath
appName = "lumberhack"

appDataDirectory :: IO FilePath
appDataDirectory = getXdgDirectory XdgData appName

-- NOTE: This is exists 
storeDataDirectory :: IO FilePath
storeDataDirectory =  appDataDirectory <&> (</> "store")

appDataFile :: IO FilePath
appDataFile =  appDataDirectory <&> (</> "appdata")

taggedInodesDataFile :: IO FilePath
taggedInodesDataFile =  appDataDirectory <&> (</> "tagged_inodes")

tagsDataFile :: IO FilePath
tagsDataFile =  appDataDirectory <&> (</> "tags_list")

initAppDirectory = do
    storeDataDirectory >>= createDirectoryIfMissing True
    let initAppFile f = doesFileExist f >>= (`unless` writeFile f (fileWarningMessage ++ "\n"))
    taggedInodesDataFile >>= initAppFile
    tagsDataFile >>= initAppFile
