module Service.Feedback.Connector.GitHub.GitHubMapper where

import Data.Maybe
import qualified Data.Text as T
import qualified GitHub.Data.Issues as GI
import qualified GitHub.Data.Definitions as GD

import Model.Feedback.SimpleIssue

toSimpleIssue :: GI.Issue -> SimpleIssue
toSimpleIssue i =
  SimpleIssue
  { _simpleIssueIssueId = GD.unIssueNumber . GI.issueNumber $ i
  , _simpleIssueTitle = T.unpack $ GI.issueTitle i
  , _simpleIssueContent = fromMaybe "" (T.unpack <$> GI.issueBody i)
  }
