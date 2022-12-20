module Wizard.Service.Submission.SubmissionUtil where

import qualified Data.List as L
import qualified Data.UUID as U

import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Submission.Submission
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Submission.SubmissionMapper

enhanceSubmission :: Submission -> AppContextM SubmissionDTO
enhanceSubmission sub = do
  user <- findUserById (U.toString sub.createdBy)
  appConfig <- getAppConfig
  let mSubmissionName =
        fmap (.name) . L.find (\s -> s.sId == sub.serviceId) $ appConfig.submission.services
  return $ toDTO sub mSubmissionName user
