module Service.BookReference.BookReferenceService where

import Api.Resource.BookReference.BookReferenceDTO
import Database.DAO.BookReference.BookReferenceDAO
import Model.Context.AppContext
import Model.Error.Error
import Service.BookReference.BookReferenceMapper

getBookReference :: String -> AppContextM (Either AppError BookReferenceDTO)
getBookReference shortUuid = heFindBookReferenceByShortUuid shortUuid $ \br -> return . Right . toDTO $ br
