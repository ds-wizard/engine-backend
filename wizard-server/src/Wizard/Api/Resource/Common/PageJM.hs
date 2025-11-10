module Wizard.Api.Resource.Common.PageJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Api.Resource.Common.PageJM ()
import Shared.Common.Api.Resource.Common.PageMetadataJM ()
import Shared.Common.Model.Common.Page
import Shared.Common.Util.JSON
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList

instance FromJSON (Page KnowledgeModelEditorList) where
  parseJSON (Object o) = do
    page <- o .: "page"
    embedded <- o .: "_embedded" .-> "knowledgeModelEditors"
    return $ Page "knowledgeModelEditors" page embedded
  parseJSON _ = mzero
