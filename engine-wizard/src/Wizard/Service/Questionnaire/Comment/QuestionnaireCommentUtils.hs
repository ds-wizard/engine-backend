module Wizard.Service.Questionnaire.Comment.QuestionnaireCommentUtils where

import Control.Lens ((^.))
import Control.Monad.Except (catchError)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.QuestionnaireAcl

enhanceQuestionnaireCommentThread :: QuestionnaireCommentThread -> AppContextM QuestionnaireCommentThreadDTO
enhanceQuestionnaireCommentThread thread = do
  mUser <-
    case thread ^. createdBy of
      Just userUuid -> findUserById' (U.toString userUuid)
      Nothing -> return Nothing
  commentsDto <- traverse enhanceQuestionnaireComment (thread ^. comments)
  return $ toCommentThreadDTO thread mUser commentsDto

enhanceQuestionnaireComment :: QuestionnaireComment -> AppContextM QuestionnaireCommentDTO
enhanceQuestionnaireComment comment = do
  mUser <-
    case comment ^. createdBy of
      Just userUuid -> findUserById' (U.toString userUuid)
      Nothing -> return Nothing
  return $ toCommentDTO comment mUser

filterComments :: Questionnaire -> [QuestionnaireCommentThread] -> AppContextM [QuestionnaireCommentThread]
filterComments qtn threads =
  catchError
    (do checkEditPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
        return threads)
    (\_ ->
       catchError
         (do checkCommentPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
             return . filter (\t -> not (t ^. private)) $ threads)
         (\_ -> return []))
