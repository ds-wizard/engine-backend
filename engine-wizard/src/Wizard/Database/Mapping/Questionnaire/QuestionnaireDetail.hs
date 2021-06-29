module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetail where

import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Model.Package.PackageSimple
import Shared.Util.Gravatar
import Shared.Util.String
import Shared.Util.Uuid
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireDetail

instance FromRow QuestionnaireDetail where
  fromRow = do
    _questionnaireDetailUuid <- field
    _questionnaireDetailName <- field
    _questionnaireDetailDescription <- field
    _questionnaireDetailVisibility <- field
    _questionnaireDetailSharing <- field
    _questionnaireDetailSelectedTagUuids <- fieldWith fromJSONField
    _questionnaireDetailEvents <- fieldWith fromJSONField
    _questionnaireDetailIsTemplate <- field
    _questionnaireDetailCreatedAt <- field
    _questionnaireDetailUpdatedAt <- field
    _questionnaireDetailState <- field
    _questionnaireDetailPackageId <- field
    let _packageSimplePId = _questionnaireDetailPackageId
    _packageSimpleName <- field
    _packageSimpleVersion <- field
    let _questionnaireDetailPackage = PackageSimple {..}
    mPermissions <- fieldWith (optionalField fromField)
    let _questionnaireDetailPermissions =
          case mPermissions of
            Just permissions -> fmap (parsePermission _questionnaireDetailUuid) . fromPGArray $ permissions
            Nothing -> []
    return $ QuestionnaireDetail {..}
    where
      parsePermission :: U.UUID -> String -> QuestionnairePermRecordDTO
      parsePermission qtnUuid permission =
        let parts = splitOn "::" permission
         in QuestionnairePermRecordDTO
              { _questionnairePermRecordDTOUuid = u' (head parts)
              , _questionnairePermRecordDTOQuestionnaireUuid = qtnUuid
              , _questionnairePermRecordDTOPerms = splitOn "," . replace "}" "" . replace "{" "" $ parts !! 1
              , _questionnairePermRecordDTOMember =
                  UserMemberDTO
                    { _userMemberDTOUuid = u' (parts !! 2)
                    , _userMemberDTOFirstName = parts !! 3
                    , _userMemberDTOLastName = parts !! 4
                    , _userMemberDTOGravatarHash = createGravatarHash $ parts !! 5
                    , _userMemberDTOImageUrl =
                        case parts !! 6 of
                          "" -> Nothing
                          imageUrl -> Just imageUrl
                    }
              }
