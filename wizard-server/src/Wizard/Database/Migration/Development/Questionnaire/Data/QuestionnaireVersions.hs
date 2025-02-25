module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions where

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.Tenant (defaultTenantUuid)
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import Wizard.Model.User.User
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper

qVersions :: U.UUID -> [QuestionnaireVersion]
qVersions qtnUuid = [questionnaireVersion1 qtnUuid]

qVersionsList :: U.UUID -> [QuestionnaireVersionList]
qVersionsList qtnUuid = [questionnaireVersion1List qtnUuid]

questionnaireVersion1 :: U.UUID -> QuestionnaireVersion
questionnaireVersion1 qtnUuid =
  QuestionnaireVersion
    { uuid = createVersionUuid qtnUuid "dd016270ce7e"
    , name = "Version 1"
    , description = Just "Version 1 description"
    , eventUuid = (slble_rQ1 qtnUuid).uuid
    , questionnaireUuid = qtnUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireVersion1List :: U.UUID -> QuestionnaireVersionList
questionnaireVersion1List qtnUuid = toVersionList (questionnaireVersion1 qtnUuid) (Just userAlbertDto)

questionnaireVersion1Edited :: U.UUID -> QuestionnaireVersion
questionnaireVersion1Edited qtnUuid =
  (questionnaireVersion1 qtnUuid)
    { name = "EDITED: " ++ (questionnaireVersion1 qtnUuid).name
    , description = fmap ("EDITED: " ++) (questionnaireVersion1 qtnUuid).description
    , eventUuid = (sre_rQ11 qtnUuid).uuid
    }

questionnaireVersion1EditedList :: U.UUID -> QuestionnaireVersionList
questionnaireVersion1EditedList qtnUuid = toVersionList (questionnaireVersion1Edited qtnUuid) (Just userAlbertDto)

questionnaireVersion1EditedChangeDto :: U.UUID -> QuestionnaireVersionChangeDTO
questionnaireVersion1EditedChangeDto qtnUuid = toVersionChangeDTO (questionnaireVersion1Edited qtnUuid)

questionnaireVersion1RevertDto :: U.UUID -> QuestionnaireVersionRevertDTO
questionnaireVersion1RevertDto qtnUuid = toVersionRevertDTO (sre_rQ2 qtnUuid).uuid

questionnaireVersion2 :: U.UUID -> QuestionnaireVersion
questionnaireVersion2 qtnUuid =
  QuestionnaireVersion
    { uuid = createVersionUuid qtnUuid "515f1d45b24f"
    , name = "Version 2"
    , description = Just "Version 2 description"
    , eventUuid = (sre_rQ11 qtnUuid).uuid
    , questionnaireUuid = qtnUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
questionnaireVersion2ChangeDto :: U.UUID -> QuestionnaireVersionChangeDTO
questionnaireVersion2ChangeDto qtnUuid = toVersionChangeDTO (questionnaireVersion2 qtnUuid)

createVersionUuid :: U.UUID -> String -> U.UUID
createVersionUuid questionnaireUuid eventSuffix =
  let parts = splitOn "-" . U.toString $ questionnaireUuid
   in u' . L.intercalate "-" $ [head parts, parts !! 1, parts !! 2, parts !! 3, eventSuffix]
