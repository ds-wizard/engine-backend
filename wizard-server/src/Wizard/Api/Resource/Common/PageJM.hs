module Wizard.Api.Resource.Common.PageJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Api.Resource.Common.PageJM ()
import Shared.Common.Api.Resource.Common.PageMetadataJM ()
import Shared.Common.Model.Common.Page
import Shared.Common.Util.JSON
import Wizard.Api.Resource.Branch.BranchListJM ()
import Wizard.Model.Branch.BranchList

instance FromJSON (Page BranchList) where
  parseJSON (Object o) = do
    page <- o .: "page"
    embedded <- o .: "_embedded" .-> "branches"
    return $ Page "branches" page embedded
  parseJSON _ = mzero
