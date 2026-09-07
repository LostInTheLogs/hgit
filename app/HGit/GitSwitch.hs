module HGit.GitSwitch (gitSwitch, SwitchOptions (..)) where

import HGit.Branch (setHeadToBranch)
import HGit.FindObject (findAndCoerceToTree, findObject)
import HGit.Index (readIndex)
import HGit.Object (ObjType (CommitObj), Object (..), readObjOfType)
import HGit.Repository (WithRepository (WithRepository), gitPath, runWithFoundRepo)
import HGit.Tree (flattenTree)
import HGit.UnpackTree (UnpackTreeOpts (..), unpackTree)
import HGit.Utils
import Relude
import qualified UnliftIO.Directory as Dir

data SwitchOptions = SwitchOptions {optBranch :: Text}

gitSwitch :: SwitchOptions -> IO ()
gitSwitch SwitchOptions{..} = runWithFoundRepo $ do
  tree <- findAndCoerceToTree optBranch
  flattened <- flattenTree tree

  idx <- readIndex

  unpackTree UnpackTreeOpts{utoCheckConflicts = True} idx flattened
  setHeadToBranch optBranch
