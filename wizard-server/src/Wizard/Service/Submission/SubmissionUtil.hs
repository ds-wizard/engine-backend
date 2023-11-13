module Wizard.Service.Submission.SubmissionUtil where

import qualified Data.List as L

import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Submission.Submission
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Submission.SubmissionMapper
import Wizard.Service.Tenant.Config.ConfigService

enhanceSubmission :: Submission -> AppContextM SubmissionDTO
enhanceSubmission sub = do
  user <- findUserByUuid sub.createdBy
  tenantConfig <- getCurrentTenantConfig
  let mSubmissionName =
        fmap (.name) . L.find (\s -> s.sId == sub.serviceId) $ tenantConfig.submission.services
  return $ toDTO sub mSubmissionName user
