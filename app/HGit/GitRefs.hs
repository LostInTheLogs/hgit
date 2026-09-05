{-# LANGUAGE RecordWildCards #-}

module HGit.GitRefs (gitRefsList) where

import qualified Data.Text as T
import HGit.Object (Object (objType), readObj)
import HGit.Ref (collectRefs)
import HGit.Repository (WithRepository, runWithFoundRepo)
import HGit.Types (hashToAscii)
import HGit.Utils
import Relude

gitRefsList :: IO ()
gitRefsList = runWithFoundRepo $ do
  refs <- collectRefs
  forM_ refs $ \(hash, path) -> do
    obj <- readObj hash
    putBS $ hashToAscii hash
    putBS " "
    putText $ T.justifyLeft 7 ' ' $ show $ objType obj
    putStrLn path
