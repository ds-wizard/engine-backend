module Api.Handler.Migration.Questionnaire.MigratorHandler where

import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Migration.Questionnaire.MigratorStateChangeJM
       ()
import Api.Resource.Migration.Questionnaire.MigratorStateCreateJM
       ()
import Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Service.Migration.Questionnaire.MigratorService

postQuestionnaireMigrationsA :: Endpoint
postQuestionnaireMigrationsA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      qtnUuid <- param "qtnUuid"
      eitherResDto <- runInAuthService $ createQuestionnaireMigration qtnUuid reqDto
      case eitherResDto of
        Right resDto -> do
          status created201
          json resDto
        Left error -> sendError error

getQuestionnaireMigrationsCurrentA :: Endpoint
getQuestionnaireMigrationsCurrentA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    qtnUuid <- param "qtnUuid"
    eitherResDto <- runInAuthService $ getQuestionnaireMigration qtnUuid
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error

putQuestionnaireMigrationsCurrentA :: Endpoint
putQuestionnaireMigrationsCurrentA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      qtnUuid <- param "qtnUuid"
      eitherResDto <- runInAuthService $ modifyQuestionnaireMigration qtnUuid reqDto
      case eitherResDto of
        Right resDto -> json resDto
        Left error -> sendError error

deleteQuestionnaireMigrationsCurrentA :: Endpoint
deleteQuestionnaireMigrationsCurrentA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    qtnUuid <- param "qtnUuid"
    result <- runInAuthService $ finishQuestionnaireMigration qtnUuid
    case result of
      Nothing -> status noContent204
      Just error -> sendError error
