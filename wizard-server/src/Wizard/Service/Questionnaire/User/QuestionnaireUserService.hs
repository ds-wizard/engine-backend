module Wizard.Service.Questionnaire.User.QuestionnaireUserService where

import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireUserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

getQuestionnaireUserSuggestionsPage :: U.UUID -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page UserSuggestionDTO)
getQuestionnaireUserSuggestionsPage qtnUuid mQuery mEditor pageable sort = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkCommentPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  if qtn.visibility == VisibleCommentQuestionnaire || qtn.visibility == VisibleEditQuestionnaire || qtn.sharing == AnyoneWithLinkCommentQuestionnaire || qtn.sharing == AnyoneWithLinkEditQuestionnaire
    then getUserSuggestionsPage mQuery Nothing Nothing pageable sort
    else do
      let perm =
            case mEditor of
              Just True -> "EDIT"
              _ -> "COMMENT"
      suggestionPage <- findQuestionnaireUserSuggestionsPage qtnUuid perm mQuery pageable sort
      return . fmap toSuggestionDTO $ suggestionPage
