module Wizard.Api.Resource.Common.PageJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Model.Common.Page
import Shared.Util.JSON
import Wizard.Api.Resource.Branch.BranchListJM ()
import Wizard.Model.Branch.BranchList

instance FromJSON (Page BranchList) where
  parseJSON (Object o) = do
    page <- o .: "page"
    embedded <- o .: "_embedded" .-> "branches"
    return $ Page "branches" page embedded
  parseJSON _ = mzero
