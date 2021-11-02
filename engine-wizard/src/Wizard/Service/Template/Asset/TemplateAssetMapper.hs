module Wizard.Service.Template.Asset.TemplateAssetMapper where

import qualified Data.UUID as U

import Shared.Model.Template.Template

fromChangeDTO :: String -> U.UUID -> String -> String -> U.UUID -> TemplateAsset
fromChangeDTO tmlId aUuid fileName contentType appUuid =
  TemplateAsset
    { _templateAssetTemplateId = tmlId
    , _templateAssetUuid = aUuid
    , _templateAssetFileName = fileName
    , _templateAssetContentType = contentType
    , _templateAssetAppUuid = appUuid
    }
