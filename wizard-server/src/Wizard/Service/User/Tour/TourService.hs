module Wizard.Service.User.Tour.TourService where

import Control.Monad (void)
import qualified Data.UUID as U

import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.DAO.User.UserTourDAO

deleteTours :: U.UUID -> AppContextM ()
deleteTours userUuid = void $ deleteToursByUserUuid userUuid
