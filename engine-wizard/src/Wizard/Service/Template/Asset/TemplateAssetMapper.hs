module Wizard.Service.Template.Asset.TemplateAssetMapper where

import qualified Data.UUID as U
import GHC.Int

import Shared.Model.Template.Template

fromChangeDTO :: String -> U.UUID -> String -> String -> Int64 -> U.UUID -> TemplateAsset
fromChangeDTO tmlId aUuid fileName contentType fileSize appUuid =
  TemplateAsset
    { _templateAssetTemplateId = tmlId
    , _templateAssetUuid = aUuid
    , _templateAssetFileName = fileName
    , _templateAssetContentType = contentType
    , _templateAssetFileSize = fileSize
    , _templateAssetAppUuid = appUuid
    }
