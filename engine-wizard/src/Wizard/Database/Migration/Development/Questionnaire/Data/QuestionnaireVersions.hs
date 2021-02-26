module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper

qVersions :: [QuestionnaireVersion]
qVersions = [questionnaireVersion1]

qVersionsDto :: [QuestionnaireVersionDTO]
qVersionsDto = [questionnaireVersion1Dto]

questionnaireVersion1 :: QuestionnaireVersion
questionnaireVersion1 =
  QuestionnaireVersion
    { _questionnaireVersionUuid = u' "bd6611c8-ea11-48ab-adaa-3ce51b66aae5"
    , _questionnaireVersionName = "Version 1"
    , _questionnaireVersionDescription = Just "Version 1 description"
    , _questionnaireVersionEventUuid = slble_rQ1 ^. uuid
    , _questionnaireVersionCreatedBy = userAlbert ^. uuid
    , _questionnaireVersionCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _questionnaireVersionUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

questionnaireVersion1Dto :: QuestionnaireVersionDTO
questionnaireVersion1Dto = toVersionDTO questionnaireVersion1 userAlbert

questionnaireVersion1ChangeDto :: QuestionnaireVersionChangeDTO
questionnaireVersion1ChangeDto = toVersionChangeDTO questionnaireVersion1

questionnaireVersion1Edited :: QuestionnaireVersion
questionnaireVersion1Edited =
  questionnaireVersion1
    { _questionnaireVersionName = "EDITED: " ++ questionnaireVersion1 ^. name
    , _questionnaireVersionDescription = fmap ("EDITED: " ++) (questionnaireVersion1 ^. description)
    , _questionnaireVersionEventUuid = sre_rQ11 ^. uuid
    }

questionnaireVersion1EditedDto :: QuestionnaireVersionDTO
questionnaireVersion1EditedDto = toVersionDTO questionnaireVersion1Edited userAlbert

questionnaireVersion1EditedChangeDto :: QuestionnaireVersionChangeDTO
questionnaireVersion1EditedChangeDto = toVersionChangeDTO questionnaireVersion1Edited

questionnaireVersion1RevertDto :: QuestionnaireVersionRevertDTO
questionnaireVersion1RevertDto = toVersionRevertDTO (sre_rQ2 ^. uuid)
