module Wizard.Model.PersistentCommand.DocumentTemplate.Asset.DocumentTemplateAssetDeleteFromS3Command where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data DocumentTemplateAssetDeleteFromS3Command = DocumentTemplateAssetDeleteFromS3Command
  { documentTemplateId :: String
  , assetUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON DocumentTemplateAssetDeleteFromS3Command where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetDeleteFromS3Command where
  toJSON = genericToJSON jsonOptions
