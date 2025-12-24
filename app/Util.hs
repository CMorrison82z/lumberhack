module Util where

import Debug.Trace
import System.OsPath (OsPath)
import qualified System.OsPath as OSP
import System.Directory.OsPath.Streaming (FileType (File), SymlinkType (Regular), getDirectoryContentsWithFilterRecursive)
import System.IO.Unsafe (unsafeInterleaveIO)

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                   0 -> Just x
                   _ -> r (k-1)) (const Nothing) xs n

dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ []     = return []
dropWhileM p (x:xs) = do
    q <- p x
    if q then dropWhileM p xs
    else return (x:xs)

applyFstToSnd (f, x) = f x

filterMLazy :: (a -> IO Bool) -> [a] -> IO [a]
filterMLazy p xs = unsafeInterleaveIO $ case xs of
  [] -> return []
  (x:xs') -> do
    xs'' <- filterMLazy p xs'
    isP <- p x
    return $ if isP
        then (x : xs'')
        else xs''

getFilesRecursiveAbs :: OsPath -> IO [OsPath]
getFilesRecursiveAbs osp = 
    fmap
        ( 
            fmap ( (osp OSP.</>) . fst )
            . filter (\(_, t) -> t == File Regular)
        )
    $ getDirectoryContentsWithFilterRecursive (\_ s -> s == Regular) (const True) osp

quotedWords :: String -> [String]
quotedWords s = go [] s
  where
    go acc remainder = case span (\x -> x /= ',' && x /= '"') remainder of
      (e, ',' : rest) -> go (acc ++ [e]) rest
      (e, '"' : rest) -> case span (/= '"') rest of
        -- Matches the case when there are subsequent elements after `e2`, and we need to drop the `','` as we do above
        (e2, _ : ',' : rest2) -> go (acc ++ [e ++ e2]) rest2
        (e2, _ : rest2) -> go (acc ++ [e ++ e2]) rest2
        (_, []) -> error "An element had no closing '\"'"
      (_, _ : _) -> undefined
      ([], []) -> acc
      (last_elem, []) -> acc ++ [last_elem]

-- WARN: Do NOT use `foldr1`, as a subtle bug is introduced in that the first element of `ws` will NOT be caught when determining if it should be surrounded !
wordsToCsv [] = ""
wordsToCsv ws = foldr lam "" ws
    where
        lam w "" = if elem ',' w then surroundList w '"' else w
        lam w acc = (if elem ',' w then surroundList w '"' else w) ++ ',' : acc

surroundList xs x = x:xs ++ [x]
