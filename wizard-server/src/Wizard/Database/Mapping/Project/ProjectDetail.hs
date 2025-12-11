module Wizard.Database.Mapping.Project.ProjectDetail where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.Project.Event.ProjectEventJM ()
import Wizard.Database.Mapping.Project.ProjectAcl
import Wizard.Database.Mapping.Project.ProjectSharing ()
import Wizard.Database.Mapping.Project.ProjectState ()
import Wizard.Database.Mapping.Project.ProjectVisibility ()
import Wizard.Model.Project.Detail.ProjectDetail

instance FromRow ProjectDetail where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackageId <- field
    selectedQuestionTagUuids <- fromPGArray <$> field
    isTemplate <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    projectActionsAvailable <- field
    projectImportersAvailable <- field
    fileCount <- field
    return $ ProjectDetail {..}
