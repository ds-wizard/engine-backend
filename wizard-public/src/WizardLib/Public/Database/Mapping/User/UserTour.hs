module WizardLib.Public.Database.Mapping.User.UserTour where

import Database.PostgreSQL.Simple

import WizardLib.Public.Model.User.UserTour

instance ToRow UserTour

instance FromRow UserTour
