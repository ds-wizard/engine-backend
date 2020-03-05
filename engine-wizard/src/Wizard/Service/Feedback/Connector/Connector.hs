module Wizard.Service.Feedback.Connector.Connector where

import qualified Data.UUID as U

import Wizard.Model.Feedback.SimpleIssue

class Connector a where
  getIssues :: a [SimpleIssue]
  createIssue :: String -> U.UUID -> String -> String -> a Int
