module Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentUtils

getQuestionnaireComments :: Questionnaire -> AppContextM (M.Map String [QuestionnaireCommentThreadDTO])
getQuestionnaireComments qtn = do
  threads <- findQuestionnaireCommentThreads (U.toString $ qtn ^. uuid)
  filteredThreads <- filterComments qtn threads
  threadsDto <- traverse enhanceQuestionnaireCommentThread filteredThreads
  let commentThreadsMap = toCommentThreadsMap threadsDto
  return commentThreadsMap
