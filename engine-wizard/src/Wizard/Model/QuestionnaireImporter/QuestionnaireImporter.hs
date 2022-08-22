module Wizard.Model.QuestionnaireImporter.QuestionnaireImporter where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Package.PackagePattern

data QuestionnaireImporter =
  QuestionnaireImporter
    { _questionnaireImporterQiId :: String
    , _questionnaireImporterName :: String
    , _questionnaireImporterOrganizationId :: String
    , _questionnaireImporterImporterId :: String
    , _questionnaireImporterVersion :: String
    , _questionnaireImporterMetamodelVersion :: Int
    , _questionnaireImporterDescription :: String
    , _questionnaireImporterReadme :: String
    , _questionnaireImporterLicense :: String
    , _questionnaireImporterAllowedPackages :: [PackagePattern]
    , _questionnaireImporterUrl :: String
    , _questionnaireImporterEnabled :: Bool
    , _questionnaireImporterAppUuid :: U.UUID
    , _questionnaireImporterCreatedAt :: UTCTime
    , _questionnaireImporterUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
