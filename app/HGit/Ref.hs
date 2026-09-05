{-# LANGUAGE ScopedTypeVariables #-}

module HGit.Ref where

import Data.Foldable.Extra (findM)
import qualified Data.List as List
import HGit.Repository (Repository (repoGitdir), WithRepository, gitPath, gitPath')
import HGit.Types (Hash, asciiToHash)
import HGit.Utils
import Relude
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)
import UnliftIO (IOException)
import qualified UnliftIO.Directory as Dir
import UnliftIO.Exception (catch)

canonicalizeSymRef :: FilePath -> WithRepository FilePath
canonicalizeSymRef path = do
  refOrHead <- fReadStrLine path
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return path
    Just ref -> asks gitPath [ref] >>= canonicalizeSymRef

followRef :: FilePath -> WithRepository Hash
followRef path = do
  refOrHead <- fReadStrLine path
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return $ asciiToHash refOrHead
    Just ref -> asks gitPath [ref] >>= followRef

resolveRef :: FilePath -> WithRepository (Maybe Hash)
resolveRef name = do
  let relPaths =
        [ name
        , "refs" </> name
        , "refs" </> "tags" </> name
        , "refs" </> "heads" </> name
        , "refs" </> "remotes" </> name
        , "refs" </> "remotes" </> name </> "HEAD"
        ]

  paths <- mapM gitPath' relPaths
  found <- findM Dir.doesFileExist paths
  case found of
    Nothing -> return Nothing
    Just path -> Just <$> followRef path

collectRefs :: WithRepository [(Hash, String)]
collectRefs = do
  gitDir <- asks repoGitdir
  files <- liftIO $ getDirectoryFiles gitDir ["refs/heads/*", "refs/tags/*", "refs/remotes/*/*"]

  heads <- forM files $ \relPath -> do
    path <- gitPath' relPath
    hash <- followRef path
    return (hash, relPath)

  pass -- TODO: packed-refs
  return heads
