module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetail where

import qualified Data.List as L
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Gravatar
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireDetail
import WizardLib.KnowledgeModel.Model.Package.PackageSimple

instance FromRow QuestionnaireDetail where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    visibility <- field
    sharing <- field
    selectedQuestionTagUuids <- fieldWith fromJSONField
    events <- fieldWith fromJSONField
    isTemplate <- field
    answeredQuestions <- field
    unansweredQuestions <- field
    createdAt <- field
    updatedAt <- field
    state <- field
    packageId <- field
    packageName <- field
    packageVersion <- field
    let package = PackageSimple {pId = packageId, name = packageName, version = packageVersion}
    mPermissions <- fieldWith (optionalField fromField)
    let permissions =
          case mPermissions of
            Just permissions -> L.sort . fmap (parsePermission uuid) . fromPGArray $ permissions
            Nothing -> []
    return $ QuestionnaireDetail {..}
    where
      parsePermission :: U.UUID -> String -> QuestionnairePermRecordDTO
      parsePermission qtnUuid permission =
        let parts = splitOn "::" permission
         in QuestionnairePermRecordDTO
              { uuid = u' (head parts)
              , questionnaireUuid = qtnUuid
              , perms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
              , member =
                  UserMemberDTO
                    { uuid = u' (parts !! 2)
                    , firstName = parts !! 3
                    , lastName = parts !! 4
                    , gravatarHash = createGravatarHash $ parts !! 5
                    , imageUrl =
                        case parts !! 6 of
                          "" -> Nothing
                          imageUrl -> Just imageUrl
                    }
              }
