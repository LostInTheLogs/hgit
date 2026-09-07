module HGit.Branch where

import qualified Data.List as List
import HGit.FindObject (readFindObjOfType)
import HGit.Object (ObjType (..))
import HGit.Repository (WithRepository, gitPath)
import HGit.Types (Object (..))
import HGit.Utils
import Relude
import qualified UnliftIO.Directory as Dir

-- | returns either the branch (Right) or detached head (Left)
getBranch :: WithRepository (Either String String)
getBranch = do
  refOrHead <- fReadStrLine =<< gitPath ["HEAD"]
  case List.stripPrefix "ref: refs/heads/" refOrHead of
    Nothing -> return $ Left refOrHead -- detached head
    Just ref -> return $ Right ref -- branch

setHeadToBranch :: Text -> WithRepository ()
setHeadToBranch ref = do
  let branchRef = "refs/heads/" <> ref
  branchExists <- Dir.doesFileExist =<< gitPath [toString branchRef]
  newHead <-
    if branchExists
      then return $ "ref: " <> branchRef
      else show . objHash <$> readFindObjOfType CommitObj ref

  headPath <- gitPath ["HEAD"]
  writeFileText headPath newHead
