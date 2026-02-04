module Wizard.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

instance FromRow KnowledgeModelPackageSuggestion where
  fromRow = do
    uuid <- field
    name <- field
    organizationId <- field
    kmId <- field
    version <- field
    description <- field
    return $ KnowledgeModelPackageSuggestion {..}

fieldKnowledgeModelPackageSuggestion :: RowParser KnowledgeModelPackageSuggestion
fieldKnowledgeModelPackageSuggestion = do
  uuid <- field
  name <- field
  organizationId <- field
  kmId <- field
  version <- field
  description <- field
  return KnowledgeModelPackageSuggestion {..}

fieldKnowledgeModelPackageSuggestion' :: RowParser (Maybe KnowledgeModelPackageSuggestion)
fieldKnowledgeModelPackageSuggestion' = do
  mUuid <- field
  mName <- field
  mOrganizationId <- field
  mKmId <- field
  mVersion <- field
  mDescription <- field
  case (mUuid, mName, mOrganizationId, mKmId, mVersion, mDescription) of
    (Just uuid, Just name, Just organizationId, Just kmId, Just version, Just description) -> return $ Just KnowledgeModelPackageSuggestion {..}
    _ -> return Nothing
