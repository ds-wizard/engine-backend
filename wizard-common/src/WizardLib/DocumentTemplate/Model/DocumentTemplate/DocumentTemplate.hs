module WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import WizardLib.KnowledgeModel.Model.Package.PackagePattern

data DocumentTemplatePhase
  = DraftDocumentTemplatePhase
  | ReleasedDocumentTemplatePhase
  | DeprecatedDocumentTemplatePhase
  deriving (Show, Eq, Generic, Read)

data DocumentTemplate = DocumentTemplate
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  , nonEditable :: Bool
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFormat = DocumentTemplateFormat
  { uuid :: U.UUID
  , name :: String
  , icon :: String
  , steps :: [DocumentTemplateFormatStep]
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFormatStep = DocumentTemplateFormatStep
  { name :: String
  , options :: M.Map String String
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFile = DocumentTemplateFile
  { documentTemplateId :: String
  , uuid :: U.UUID
  , fileName :: String
  , content :: String
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateAsset = DocumentTemplateAsset
  { documentTemplateId :: String
  , uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplate where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.templateId b.templateId
      <> compare a.version b.version
