module Wizard.Service.Template.Asset.TemplateAssetMapper where

import qualified Data.UUID as U
import GHC.Int

import Shared.Model.Template.Template

fromChangeDTO :: String -> U.UUID -> String -> String -> Int64 -> U.UUID -> TemplateAsset
fromChangeDTO tmlId aUuid fileName contentType fileSize appUuid =
  TemplateAsset
    { templateId = tmlId
    , uuid = aUuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    , appUuid = appUuid
    }
