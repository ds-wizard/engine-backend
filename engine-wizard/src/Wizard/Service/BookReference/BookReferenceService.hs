module Wizard.Service.BookReference.BookReferenceService where

import Shared.Model.Error.Error
import Wizard.Api.Resource.BookReference.BookReferenceDTO
import Wizard.Database.DAO.BookReference.BookReferenceDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.BookReference.BookReferenceMapper

getBookReference :: String -> AppContextM (Either AppError BookReferenceDTO)
getBookReference shortUuid = heFindBookReferenceByShortUuid shortUuid $ \br -> return . Right . toDTO $ br
