module Wizard.Database.Mapping.Project.ProjectDetailPreview where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Database.Mapping.Project.ProjectAcl
import Wizard.Database.Mapping.Project.ProjectSharing ()
import Wizard.Database.Mapping.Project.ProjectVisibility ()
import Wizard.Model.Project.Detail.ProjectDetailPreview

instance FromRow ProjectDetailPreview where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackageId <- field
    isTemplate <- field
    documentTemplateId <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    mFormatUuid <- field
    mFormatName <- field
    mFormatIcon <- field
    let format =
          case (mFormatUuid, mFormatName, mFormatIcon) of
            (Just uuid, Just name, Just icon) -> Just $ DocumentTemplateFormatSimple {uuid = uuid, name = name, icon = icon}
            _ -> Nothing
    fileCount <- field
    return $ ProjectDetailPreview {..}
