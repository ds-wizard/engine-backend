module Wizard.Database.Mapping.Feedback.Feedback where

import Database.PostgreSQL.Simple

import Wizard.Model.Feedback.Feedback

instance ToRow Feedback

instance FromRow Feedback
