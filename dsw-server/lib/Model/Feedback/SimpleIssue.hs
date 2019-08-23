module Model.Feedback.SimpleIssue where

data SimpleIssue = SimpleIssue
  { _simpleIssueIssueId :: Int
  , _simpleIssueTitle :: String
  , _simpleIssueContent :: String
  } deriving (Show, Eq)
