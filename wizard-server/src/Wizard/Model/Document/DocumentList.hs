module Wizard.Model.Document.DocumentList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Wizard.Model.Document.Document
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

data DocumentList = DocumentList
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , questionnaireUuid :: U.UUID
  , questionnaireName :: String
  , questionnaireEventUuid :: Maybe U.UUID
  , documentTemplateName :: String
  , documentTemplateFormats :: [DocumentTemplateFormat]
  , formatUuid :: U.UUID
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
