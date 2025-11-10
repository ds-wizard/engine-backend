module Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.Common.Model.Common.SemVer2Tuple
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

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
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , nonEditable :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFormat = DocumentTemplateFormat
  { documentTemplateId :: String
  , uuid :: U.UUID
  , name :: String
  , icon :: String
  , steps :: [DocumentTemplateFormatStep]
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplateFormat where
  compare f1 f2 = compare f1.name f2.name

data DocumentTemplateFormatStep = DocumentTemplateFormatStep
  { documentTemplateId :: String
  , formatUuid :: U.UUID
  , position :: Int
  , name :: String
  , options :: M.Map String String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFile = DocumentTemplateFile
  { documentTemplateId :: String
  , uuid :: U.UUID
  , fileName :: String
  , content :: String
  , tenantUuid :: U.UUID
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
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplate where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.templateId b.templateId
      <> compare a.version b.version
