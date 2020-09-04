module Wizard.Service.Questionnaire.Compiler.CompilerService where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireMapper

processEvent :: String -> QuestionnaireEventDTO -> AppContextM ()
processEvent qtnUuid event = do
  qtn <- findQuestionnaireById qtnUuid
  let updatedQtn = applyEvent qtn event
  updateQuestionnaireById updatedQtn

applyEvent :: Questionnaire -> QuestionnaireEventDTO -> Questionnaire
applyEvent qtn (SetReplyEventDTO' event) =
  let newReplies = M.insert (event ^. path) (fromReplyValueDTO $ event ^. value) (qtn ^. replies)
   in qtn & replies .~ newReplies
applyEvent qtn (ClearReplyEventDTO' event) =
  let newReplies = M.delete (event ^. path) (qtn ^. replies)
   in qtn & replies .~ newReplies
applyEvent qtn (SetLevelEventDTO' event) =
  let newLevel = event ^. level
   in qtn & level .~ newLevel
applyEvent qtn (SetLabelsEventDTO' event) =
  let newLabels =
        case event ^. value of
          [] -> M.delete (event ^. path) (qtn ^. labels)
          newValue -> M.insert (event ^. path) newValue (qtn ^. labels)
   in qtn & labels .~ newLabels
