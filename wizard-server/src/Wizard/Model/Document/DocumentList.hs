module Wizard.Model.Document.DocumentList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Wizard.Model.Document.Document
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

data DocumentList = DocumentList
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , questionnaireUuid :: U.UUID
  , questionnaireName :: String
  , questionnaireEventUuid :: Maybe U.UUID
  , questionnaireVersion :: Maybe String
  , documentTemplateId :: String
  , documentTemplateName :: String
  , documentTemplateFormats :: [DocumentTemplateFormatSimple]
  , formatUuid :: U.UUID
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
