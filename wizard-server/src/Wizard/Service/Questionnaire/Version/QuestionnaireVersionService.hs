module Wizard.Service.Questionnaire.Version.QuestionnaireVersionService where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtil
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionValidation

getVersions :: U.UUID -> AppContextM [QuestionnaireVersionDTO]
getVersions qtnUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  traverse enhanceQuestionnaireVersion qtn.versions

createVersion :: U.UUID -> QuestionnaireVersionChangeDTO -> AppContextM QuestionnaireVersionDTO
createVersion qtnUuid reqDto =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkOwnerPermissionToQtn qtn.visibility qtn.permissions
    validateQuestionnaireVersion reqDto qtn
    vUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let version = fromVersionChangeDTO reqDto vUuid currentUser.uuid now now
    let updatedQtn = qtn {versions = qtn.versions ++ [version]} :: Questionnaire
    updateQuestionnaireByUuid updatedQtn
    enhanceQuestionnaireVersion version

modifyVersion :: U.UUID -> U.UUID -> QuestionnaireVersionChangeDTO -> AppContextM QuestionnaireVersionDTO
modifyVersion qtnUuid vUuid reqDto =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkOwnerPermissionToQtn qtn.visibility qtn.permissions
    validateQuestionnaireVersion reqDto qtn
    now <- liftIO getCurrentTime
    version <-
      case L.find (\v -> v.uuid == vUuid) qtn.versions of
        Just version -> return version
        Nothing -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "version" [("uuid", U.toString vUuid)]
    let updatedVersion = fromVersionChangeDTO reqDto version.uuid version.createdBy version.createdAt now
    let updatedVersions =
          foldl
            ( \acc v ->
                if v.uuid == updatedVersion.uuid
                  then acc ++ [updatedVersion]
                  else acc ++ [v]
            )
            []
            qtn.versions
    let updatedQtn = qtn {versions = updatedVersions} :: Questionnaire
    updateQuestionnaireByUuid updatedQtn
    enhanceQuestionnaireVersion updatedVersion

deleteVersion :: U.UUID -> U.UUID -> AppContextM ()
deleteVersion qtnUuid vUuid =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkOwnerPermissionToQtn qtn.visibility qtn.permissions
    updatedVersions <-
      case L.find (\v -> v.uuid == vUuid) qtn.versions of
        Just version -> return $ L.delete version qtn.versions
        Nothing -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "version" [("uuid", U.toString vUuid)]
    let updatedQtn = qtn {versions = updatedVersions} :: Questionnaire
    updateQuestionnaireByUuid updatedQtn

revertToEvent :: U.UUID -> QuestionnaireVersionRevertDTO -> Bool -> AppContextM QuestionnaireContentDTO
revertToEvent qtnUuid reqDto shouldSave =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    when shouldSave (checkOwnerPermissionToQtn qtn.visibility qtn.permissions)
    let updatedEvents = takeWhileInclusive (\e -> getUuid e /= reqDto.eventUuid) qtn.events
    let updatedEventUuids = S.fromList . fmap getUuid $ updatedEvents
    let updatedVersions = filter (\v -> S.member v.eventUuid updatedEventUuids) qtn.versions
    let updatedQtn = qtn {events = updatedEvents, versions = updatedVersions} :: Questionnaire
    when shouldSave (updateQuestionnaireByUuid updatedQtn)
    qtnCtn <- compileQuestionnaire updatedQtn
    eventsDto <- traverse enhanceQuestionnaireEvent updatedQtn.events
    versionDto <- traverse enhanceQuestionnaireVersion updatedQtn.versions
    when shouldSave (logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid)
    commentThreadsMap <- getQuestionnaireComments qtn
    return $ toContentDTO qtnCtn commentThreadsMap eventsDto versionDto
