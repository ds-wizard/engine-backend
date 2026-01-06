module Wizard.Database.Mapping.Project.Project where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.Event.ProjectEventJM ()
import Wizard.Database.Mapping.Project.ProjectSharing ()
import Wizard.Database.Mapping.Project.ProjectVisibility ()
import Wizard.Model.Project.Project

instance ToRow Project where
  toRow Project {..} =
    [ toField uuid
    , toField name
    , toField visibility
    , toField sharing
    , toField knowledgeModelPackageId
    , toField . PGArray $ selectedQuestionTagUuids
    , toField documentTemplateId
    , toField formatUuid
    , toField creatorUuid
    , toField createdAt
    , toField updatedAt
    , toField description
    , toField isTemplate
    , toField squashed
    , toField tenantUuid
    , toField . PGArray $ projectTags
    ]

instance FromRow Project where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackageId <- field
    selectedQuestionTagUuids <- fromPGArray <$> field
    documentTemplateId <- field
    formatUuid <- field
    creatorUuid <- field
    let permissions = []
    createdAt <- field
    updatedAt <- field
    description <- field
    isTemplate <- field
    squashed <- field
    tenantUuid <- field
    projectTags <- fromPGArray <$> field
    return $ Project {..}
