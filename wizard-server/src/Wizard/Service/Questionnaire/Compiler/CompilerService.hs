module Wizard.Service.Questionnaire.Compiler.CompilerService where

import qualified Data.Map.Strict as M

import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireContentDM
import Wizard.Model.Questionnaire.QuestionnaireEventList
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

compileQuestionnaire :: [QuestionnaireEventList] -> QuestionnaireContent
compileQuestionnaire = foldl applyEvent defaultQuestionnaireContent

applyEvent :: QuestionnaireContent -> QuestionnaireEventList -> QuestionnaireContent
applyEvent qtnCtn (SetReplyEventList' event) = qtnCtn {replies = M.insert event.path (toReply' event) qtnCtn.replies}
applyEvent qtnCtn (ClearReplyEventList' event) = qtnCtn {replies = M.delete event.path qtnCtn.replies}
applyEvent qtnCtn (SetPhaseEventList' event) = qtnCtn {phaseUuid = event.phaseUuid}
applyEvent qtnCtn (SetLabelsEventList' event) =
  qtnCtn
    { labels = case event.value of
        [] -> M.delete event.path qtnCtn.labels
        newValue -> M.insert event.path newValue qtnCtn.labels
    }
