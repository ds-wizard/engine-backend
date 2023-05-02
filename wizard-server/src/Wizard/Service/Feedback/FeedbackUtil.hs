module Wizard.Service.Feedback.FeedbackUtil where

import Shared.Common.Util.String
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Feedback.Feedback

createIssueUrl :: ServerConfigFeedback -> AppConfigQuestionnaireFeedback -> Feedback -> String
createIssueUrl serverConfig appConfig fbk =
  f' "%s/%s/%s/issues/%s" [serverConfig.webUrl, appConfig.owner, appConfig.repo, show fbk.issueId]
