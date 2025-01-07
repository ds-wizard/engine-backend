module Wizard.Model.DocumentTemplate.DocumentTemplateDraftData where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateDraftData = DocumentTemplateDraftData
  { documentTemplateId :: String
  , questionnaireUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , branchUuid :: Maybe U.UUID
  }
  deriving (Show, Generic)

instance Eq DocumentTemplateDraftData where
  a == b =
    a.documentTemplateId == b.documentTemplateId
      && a.questionnaireUuid == b.questionnaireUuid
      && a.branchUuid == b.branchUuid
      && a.formatUuid == b.formatUuid
      && a.tenantUuid == b.tenantUuid
