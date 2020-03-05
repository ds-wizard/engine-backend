module Wizard.Service.BookReference.BookReferenceService where

import Wizard.Api.Resource.BookReference.BookReferenceDTO
import Wizard.Database.DAO.BookReference.BookReferenceDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.BookReference.BookReferenceMapper

getBookReference :: String -> AppContextM BookReferenceDTO
getBookReference shortUuid = do
  br <- findBookReferenceByShortUuid shortUuid
  return . toDTO $ br
