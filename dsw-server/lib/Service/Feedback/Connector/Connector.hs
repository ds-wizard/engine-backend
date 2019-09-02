module Service.Feedback.Connector.Connector where

import qualified Data.UUID as U

import Model.Error.Error
import Model.Feedback.SimpleIssue

class Connector a where
  getIssues :: a (Either AppError [SimpleIssue])
  createIssue :: String -> U.UUID -> String -> String -> a (Either AppError Int)

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetIssues callback = do
  eIssues <- getIssues
  case eIssues of
    Right issues -> callback issues
    Left error -> return . Left $ error

hmGetIssues callback = do
  eIssues <- getIssues
  case eIssues of
    Right issues -> callback issues
    Left error -> return . Just $ error

-- --------------------------------
heCreateIssue packageId questionUuid title content callback = do
  eIssueId <- createIssue packageId questionUuid title content
  case eIssueId of
    Right issueId -> callback issueId
    Left error -> return . Left $ error

hmCreateIssue packageId questionUuid title content callback = do
  eIssueId <- createIssue packageId questionUuid title content
  case eIssueId of
    Right issueId -> callback issueId
    Left error -> return . Just $ error
