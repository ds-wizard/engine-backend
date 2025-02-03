module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailQuestionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl
import Wizard.Database.Mapping.Questionnaire.QuestionnaireFileSimple ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireDetailQuestionnaire
import Wizard.Model.Questionnaire.QuestionnaireFileSimple

instance FromRow QuestionnaireDetailQuestionnaire where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    packageId <- field
    selectedQuestionTagUuids <- fieldWith fromJSONField
    isTemplate <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    questionnaireActionsAvailable <- field
    questionnaireImportersAvailable <- field
    mFiles <- fieldWith (optionalField fromField)
    let files =
          case mFiles of
            Just files -> fmap parseFile . fromPGArray $ files
            Nothing -> []
    return $ QuestionnaireDetailQuestionnaire {..}
    where
      parseFile :: String -> QuestionnaireFileSimple
      parseFile file =
        let parts = splitOn "<:::::>" file
         in QuestionnaireFileSimple
              { uuid = u' $ head parts
              , fileName = parts !! 1
              , contentType = parts !! 2
              , fileSize = read $ parts !! 3
              }
