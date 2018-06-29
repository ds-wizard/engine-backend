module Service.Feedback.Connector.Connector where

import qualified Data.UUID as U

import Common.Error

class Connector a where
  createIssue :: String -> U.UUID -> String -> String -> a (Either AppError Int)

-- --------------------------------
-- HELPERS
-- --------------------------------
heCreateIssue packageId questionUuid title content callback = do
  eIssueId <- createIssue packageId questionUuid title content
  case eIssueId of
    Right issueId -> callback issueId
    Left error -> return . Left $ error
