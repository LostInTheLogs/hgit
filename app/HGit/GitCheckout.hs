module HGit.GitCheckout (gitCheckout, CheckoutOptions (..)) where

import HGit.Branch (setHeadToBranch)
import HGit.FindObject (findAndCoerceToTree)
import HGit.Index (readIndex)
import HGit.Repository (WithRepository (WithRepository), runWithFoundRepo)
import HGit.Tree (flattenTree)
import HGit.UnpackTree (UnpackTreeOpts (..), unpackTree)
import HGit.Utils
import Relude

data CheckoutOptions = CheckoutOptions {optBranch :: Text}

gitCheckout :: CheckoutOptions -> IO ()
gitCheckout CheckoutOptions{..} = runWithFoundRepo $ do
  -- TODO: this is currenlty a gitSwitch clone

  tree <- findAndCoerceToTree optBranch
  flattened <- flattenTree tree

  idx <- readIndex

  unpackTree UnpackTreeOpts{utoCheckConflicts = True} idx flattened
  setHeadToBranch optBranch
