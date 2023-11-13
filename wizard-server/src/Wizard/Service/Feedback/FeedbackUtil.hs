module Wizard.Service.Feedback.FeedbackUtil where

import Shared.Common.Util.String
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Feedback.Feedback
import Wizard.Model.Tenant.Config.TenantConfig

createIssueUrl :: ServerConfigFeedback -> TenantConfigQuestionnaireFeedback -> Feedback -> String
createIssueUrl serverConfig tenantConfig fbk =
  f' "%s/%s/%s/issues/%s" [serverConfig.webUrl, tenantConfig.owner, tenantConfig.repo, show fbk.issueId]
