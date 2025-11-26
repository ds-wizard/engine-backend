module Wizard.Database.Mapping.Questionnaire.QuestionnaireEventList where

import Data.Maybe (fromJust)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Util.Uuid
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventList
import WizardLib.Public.Database.Mapping.User.UserSuggestion

instance FromRow QuestionnaireEventList where
  fromRow = do
    uuid <- field
    aType <- field
    mPath <- field
    createdAt <- field
    valueType <- field
    valueText <- fieldValueText
    mValueId <- field
    mValueRaw <- field
    createdBy <- fieldUserSuggestion'
    case aType of
      SetReplyEventType -> do
        return . SetReplyEventList' $
          SetReplyEventList
            { uuid = uuid
            , path = fromJust mPath
            , value = parseValue valueType valueText mValueId mValueRaw
            , createdBy = createdBy
            , createdAt = createdAt
            }
      ClearReplyEventType ->
        let path = fromJust mPath
         in return . ClearReplyEventList' $ ClearReplyEventList {..}
      SetPhaseEventType ->
        return . SetPhaseEventList' $
          SetPhaseEventList
            { uuid = uuid
            , phaseUuid = parsePhaseUuid valueText
            , createdBy = createdBy
            , createdAt = createdAt
            }
      SetLabelsEventType ->
        return . SetLabelsEventList' $
          SetLabelsEventList
            { uuid = uuid
            , path = fromJust mPath
            , value = fmap u' valueText
            , createdBy = createdBy
            , createdAt = createdAt
            }
