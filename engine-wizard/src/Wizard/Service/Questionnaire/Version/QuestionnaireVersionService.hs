module Wizard.Service.Questionnaire.Version.QuestionnaireVersionService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Error.Error
import Shared.Util.List
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionValidation

getVersions :: String -> AppContextM [QuestionnaireVersionDTO]
getVersions qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkViewPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
  traverse enhanceQuestionnaireVersion (qtn ^. versions)

createVersion :: String -> QuestionnaireVersionChangeDTO -> AppContextM QuestionnaireVersionDTO
createVersion qtnUuid reqDto = do
  qtn <- findQuestionnaireById qtnUuid
  checkOwnerPermissionToQtn (qtn ^. visibility) (qtn ^. permissions)
  validateQuestionnaireVersion reqDto qtn
  vUuid <- liftIO generateUuid
  currentUser <- getCurrentUser
  now <- liftIO getCurrentTime
  let version = fromVersionChangeDTO reqDto vUuid (currentUser ^. uuid) now now
  let updatedQtn = qtn & versions .~ (qtn ^. versions ++ [version])
  updateQuestionnaireById updatedQtn
  enhanceQuestionnaireVersion version

modifyVersion :: String -> String -> QuestionnaireVersionChangeDTO -> AppContextM QuestionnaireVersionDTO
modifyVersion qtnUuid vUuid reqDto = do
  qtn <- findQuestionnaireById qtnUuid
  checkOwnerPermissionToQtn (qtn ^. visibility) (qtn ^. permissions)
  validateQuestionnaireVersion reqDto qtn
  now <- liftIO getCurrentTime
  version <-
    case L.find (\v -> U.toString (v ^. uuid) == vUuid) (qtn ^. versions) of
      Just version -> return version
      Nothing -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "version" vUuid
  let updatedVersion = fromVersionChangeDTO reqDto (version ^. uuid) (version ^. createdBy) (version ^. createdAt) now
  let updatedVersions =
        foldl
          (\acc v ->
             if v ^. uuid == updatedVersion ^. uuid
               then acc ++ [updatedVersion]
               else acc ++ [v])
          []
          (qtn ^. versions)
  let updatedQtn = qtn & versions .~ updatedVersions
  updateQuestionnaireById updatedQtn
  enhanceQuestionnaireVersion updatedVersion

deleteVersion :: String -> String -> AppContextM ()
deleteVersion qtnUuid vUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkOwnerPermissionToQtn (qtn ^. visibility) (qtn ^. permissions)
  updatedVersions <-
    case L.find (\v -> U.toString (v ^. uuid) == vUuid) (qtn ^. versions) of
      Just version -> return $ L.delete version (qtn ^. versions)
      Nothing -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "version" vUuid
  let updatedQtn = qtn & versions .~ updatedVersions
  updateQuestionnaireById updatedQtn

revertToEvent :: String -> QuestionnaireVersionRevertDTO -> Bool -> AppContextM QuestionnaireContentDTO
revertToEvent qtnUuid reqDto shouldSave = do
  qtn <- findQuestionnaireById qtnUuid
  checkOwnerPermissionToQtn (qtn ^. visibility) (qtn ^. permissions)
  let updatedEvents = takeWhileInclusive (\e -> e ^. uuid' /= reqDto ^. eventUuid) (qtn ^. events)
  let updatedEventUuids = S.fromList . fmap (^. uuid') $ updatedEvents
  let updatedVersions = filter (\v -> S.member (v ^. eventUuid) updatedEventUuids) (qtn ^. versions)
  let updatedQtn = (events .~ updatedEvents) . (versions .~ updatedVersions) $ qtn
  when shouldSave (updateQuestionnaireById updatedQtn)
  qtnCtn <- compileQuestionnaire updatedQtn
  eventsDto <- traverse enhanceQuestionnaireEvent (updatedQtn ^. events)
  versionDto <- traverse enhanceQuestionnaireVersion (updatedQtn ^. versions)
  when shouldSave (logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid)
  return $ toContentDTO qtnCtn eventsDto versionDto
