module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents where

import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

setReplyEvent :: SetReplyEventDTO
setReplyEvent =
  SetReplyEventDTO
    { _setReplyEventDTOUuid = u' "3bbd961e-eace-4b74-967e-43eae0986894"
    , _setReplyEventDTOPath = fst rQ1Updated
    , _setReplyEventDTOValue = toReplyValueDTO . snd $ rQ1Updated
    }

clearReplyEvent :: ClearReplyEventDTO
clearReplyEvent =
  ClearReplyEventDTO
    {_clearReplyEventDTOUuid = u' "1c9c9b1b-ba64-438e-a415-c513e14de55e", _clearReplyEventDTOPath = fst rQ1}

setLevelEvent :: SetLevelEventDTO
setLevelEvent =
  SetLevelEventDTO {_setLevelEventDTOUuid = u' "3bbd961e-eace-4b74-967e-43eae0986894", _setLevelEventDTOLevel = 2}

setLabelsEvent :: SetLabelsEventDTO
setLabelsEvent =
  SetLabelsEventDTO
    { _setLabelsEventDTOUuid = u' "3bbd961e-eace-4b74-967e-43eae0986894"
    , _setLabelsEventDTOPath = fst rQ2
    , _setLabelsEventDTOValue = [fLabel1]
    }
