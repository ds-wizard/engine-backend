module Wizard.Service.BookReference.BookReferenceService where

import Wizard.Database.DAO.BookReference.BookReferenceDAO
import Wizard.Database.DAO.Common
import Wizard.Model.BookReference.BookReference
import Wizard.Model.Context.AppContext

getBookReference :: String -> AppContextM BookReference
getBookReference shortUuid = runInTransaction $ findBookReferenceByShortUuid shortUuid
