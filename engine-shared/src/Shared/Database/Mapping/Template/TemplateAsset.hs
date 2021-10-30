module Shared.Database.Mapping.Template.TemplateAsset where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Template.Template

instance ToRow TemplateAsset where
  toRow TemplateAsset {..} =
    [ toField _templateAssetTemplateId
    , toField _templateAssetUuid
    , toField _templateAssetFileName
    , toField _templateAssetContentType
    , toField _templateAssetAppUuid
    ]

instance FromRow TemplateAsset where
  fromRow = do
    _templateAssetTemplateId <- field
    _templateAssetUuid <- field
    _templateAssetFileName <- field
    _templateAssetContentType <- field
    _templateAssetAppUuid <- field
    return $ TemplateAsset {..}
