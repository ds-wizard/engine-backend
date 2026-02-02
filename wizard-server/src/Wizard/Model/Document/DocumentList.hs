module Wizard.Model.Document.DocumentList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Wizard.Model.Document.Document
import Wizard.Model.DocumentTemplate.DocumentTemplateWithCoordinate

data DocumentList = DocumentList
  { uuid :: U.UUID
  , name :: String
  , state :: DocumentState
  , projectUuid :: U.UUID
  , projectName :: String
  , projectEventUuid :: Maybe U.UUID
  , projectVersion :: Maybe String
  , documentTemplate :: DocumentTemplateWithCoordinate
  , documentTemplateFormat :: DocumentTemplateFormatSimple
  , fileSize :: Maybe Int64
  , workerLog :: Maybe String
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
