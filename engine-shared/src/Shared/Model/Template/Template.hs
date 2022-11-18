module Shared.Model.Template.Template where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Shared.Model.Package.PackagePattern

data Template = Template
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , recommendedPackageId :: Maybe String
  , formats :: [TemplateFormat]
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data TemplateFormat = TemplateFormat
  { uuid :: U.UUID
  , name :: String
  , shortName :: String
  , icon :: String
  , color :: String
  , steps :: [TemplateFormatStep]
  }
  deriving (Show, Eq, Generic)

data TemplateFormatStep = TemplateFormatStep
  { name :: String
  , options :: M.Map String String
  }
  deriving (Show, Eq, Generic)

data TemplateFile = TemplateFile
  { templateId :: String
  , uuid :: U.UUID
  , fileName :: String
  , content :: String
  , appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

data TemplateAsset = TemplateAsset
  { templateId :: String
  , uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance Ord Template where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.templateId b.templateId
      <> compare a.version b.version
