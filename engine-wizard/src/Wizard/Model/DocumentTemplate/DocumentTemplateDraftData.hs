module Wizard.Model.DocumentTemplate.DocumentTemplateDraftData where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateDraftData = DocumentTemplateDraftData
  { documentTemplateId :: String
  , questionnaireUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq DocumentTemplateDraftData where
  a == b =
    a.documentTemplateId == b.documentTemplateId
      && a.questionnaireUuid == b.questionnaireUuid
      && a.formatUuid == b.formatUuid
      && a.appUuid == b.appUuid
