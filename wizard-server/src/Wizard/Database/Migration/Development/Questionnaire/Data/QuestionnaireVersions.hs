module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions where

import Data.Maybe (fromJust)
import Data.Time

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

qVersions :: [QuestionnaireVersion]
qVersions = [questionnaireVersion1]

qVersionsDto :: [QuestionnaireVersionDTO]
qVersionsDto = [questionnaireVersion1Dto]

questionnaireVersion1 :: QuestionnaireVersion
questionnaireVersion1 =
  QuestionnaireVersion
    { uuid = u' "bd6611c8-ea11-48ab-adaa-3ce51b66aae5"
    , name = "Version 1"
    , description = Just "Version 1 description"
    , eventUuid = slble_rQ1.uuid
    , createdBy = userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireVersion1Dto :: QuestionnaireVersionDTO
questionnaireVersion1Dto = toVersionDTO questionnaireVersion1 (Just userAlbert)

questionnaireVersion1ChangeDto :: QuestionnaireVersionChangeDTO
questionnaireVersion1ChangeDto = toVersionChangeDTO questionnaireVersion1

questionnaireVersion1Edited :: QuestionnaireVersion
questionnaireVersion1Edited =
  questionnaireVersion1
    { name = "EDITED: " ++ questionnaireVersion1.name
    , description = fmap ("EDITED: " ++) questionnaireVersion1.description
    , eventUuid = sre_rQ11.uuid
    }

questionnaireVersion1EditedDto :: QuestionnaireVersionDTO
questionnaireVersion1EditedDto = toVersionDTO questionnaireVersion1Edited (Just userAlbert)

questionnaireVersion1EditedChangeDto :: QuestionnaireVersionChangeDTO
questionnaireVersion1EditedChangeDto = toVersionChangeDTO questionnaireVersion1Edited

questionnaireVersion1RevertDto :: QuestionnaireVersionRevertDTO
questionnaireVersion1RevertDto = toVersionRevertDTO sre_rQ2.uuid
