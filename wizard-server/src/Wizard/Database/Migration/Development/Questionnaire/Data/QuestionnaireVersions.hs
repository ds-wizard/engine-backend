module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.User.User
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper

qVersions :: U.UUID -> [QuestionnaireVersion]
qVersions qtnUuid = [questionnaireVersion1 qtnUuid]

qVersionsDto :: U.UUID -> [QuestionnaireVersionDTO]
qVersionsDto qtnUuid = [questionnaireVersion1Dto qtnUuid]

questionnaireVersion1 :: U.UUID -> QuestionnaireVersion
questionnaireVersion1 qtnUuid =
  QuestionnaireVersion
    { uuid = u' "bd6611c8-ea11-48ab-adaa-3ce51b66aae5"
    , name = "Version 1"
    , description = Just "Version 1 description"
    , eventUuid = (slble_rQ1 qtnUuid).uuid
    , createdBy = userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireVersion1Dto :: U.UUID -> QuestionnaireVersionDTO
questionnaireVersion1Dto qtnUuid = toVersionDTO (questionnaireVersion1 qtnUuid) (Just userAlbert)

questionnaireVersion1Edited :: U.UUID -> QuestionnaireVersion
questionnaireVersion1Edited qtnUuid =
  (questionnaireVersion1 qtnUuid)
    { name = "EDITED: " ++ (questionnaireVersion1 qtnUuid).name
    , description = fmap ("EDITED: " ++) (questionnaireVersion1 qtnUuid).description
    , eventUuid = (sre_rQ11 qtnUuid).uuid
    }

questionnaireVersion1EditedDto :: U.UUID -> QuestionnaireVersionDTO
questionnaireVersion1EditedDto qtnUuid = toVersionDTO (questionnaireVersion1Edited qtnUuid) (Just userAlbert)

questionnaireVersion1EditedChangeDto :: U.UUID -> QuestionnaireVersionChangeDTO
questionnaireVersion1EditedChangeDto qtnUuid = toVersionChangeDTO (questionnaireVersion1Edited qtnUuid)

questionnaireVersion1RevertDto :: U.UUID -> QuestionnaireVersionRevertDTO
questionnaireVersion1RevertDto qtnUuid = toVersionRevertDTO (sre_rQ2 qtnUuid).uuid

questionnaireVersion2 :: U.UUID -> QuestionnaireVersion
questionnaireVersion2 qtnUuid =
  QuestionnaireVersion
    { uuid = u' "1e57564e-77ff-438e-952c-515f1d45b24f"
    , name = "Version 2"
    , description = Just "Version 2 description"
    , eventUuid = (sre_rQ11 qtnUuid).uuid
    , createdBy = userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
questionnaireVersion2ChangeDto :: U.UUID -> QuestionnaireVersionChangeDTO
questionnaireVersion2ChangeDto qtnUuid = toVersionChangeDTO (questionnaireVersion2 qtnUuid)
