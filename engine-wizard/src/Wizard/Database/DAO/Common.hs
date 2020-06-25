module Wizard.Database.DAO.Common where

import Control.Lens ((^.))
import Data.Bson
import qualified Data.UUID as U
import Database.MongoDB (Selector)

import LensesConfig
import Shared.Database.DAO.Common
import Shared.Util.List
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.User.User

sel :: [AppContextM Selector] -> AppContextM Selector
sel = fmap concat . foldInContext

regexSel name value = return [name =: regex value]

textMaybeSel name mValue = return $ maybe [] (\value -> [name =: value]) mValue

ownerUuidSel = do
  currentUser <- getCurrentUser
  return $
    if currentUser ^. role /= _USER_ROLE_ADMIN
      then ["ownerUuid" =: U.toString (currentUser ^. uuid)]
      else []
