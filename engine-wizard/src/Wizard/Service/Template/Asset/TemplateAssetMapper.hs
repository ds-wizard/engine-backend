module Wizard.Service.Template.Asset.TemplateAssetMapper where

import qualified Data.UUID as U

import Shared.Model.Template.Template

fromChangeDTO :: String -> U.UUID -> String -> String -> TemplateAsset
fromChangeDTO tmlId aUuid fileName contentType =
  TemplateAsset
    { _templateAssetTemplateId = tmlId
    , _templateAssetUuid = aUuid
    , _templateAssetFileName = fileName
    , _templateAssetContentType = contentType
    }
