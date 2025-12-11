module Wizard.Database.Mapping.Project.ProjectDetailQuestionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Project.Event.ProjectEventJM ()
import Wizard.Database.Mapping.Project.File.ProjectFileSimple ()
import Wizard.Database.Mapping.Project.ProjectAcl
import Wizard.Database.Mapping.Project.ProjectSharing ()
import Wizard.Database.Mapping.Project.ProjectState ()
import Wizard.Database.Mapping.Project.ProjectVisibility ()
import Wizard.Model.Project.Detail.ProjectDetailQuestionnaire
import Wizard.Model.Project.File.ProjectFileSimple

instance FromRow ProjectDetailQuestionnaire where
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
    mFiles <- fieldWith (optionalField fromField)
    let files =
          case mFiles of
            Just files -> fmap parseFile . fromPGArray $ files
            Nothing -> []
    return $ ProjectDetailQuestionnaire {..}
    where
      parseFile :: String -> ProjectFileSimple
      parseFile file =
        let parts = splitOn "<:::::>" file
         in ProjectFileSimple
              { uuid = u' $ head parts
              , fileName = parts !! 1
              , contentType = parts !! 2
              , fileSize = read $ parts !! 3
              }
