module Wizard.Database.DAO.Common where

import Control.Lens ((^.))
import Data.Bson
import qualified Data.UUID as U

import LensesConfig
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.User.User

ownerUuidSel = do
  currentUser <- getCurrentUser
  return $
    if currentUser ^. role /= _USER_ROLE_ADMIN
      then ["ownerUuid" =: U.toString (currentUser ^. uuid)]
      else []
