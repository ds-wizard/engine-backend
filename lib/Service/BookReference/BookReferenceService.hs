module Service.BookReference.BookReferenceService where

import Api.Resource.BookReference.BookReferenceDTO
import Common.Error
import Database.DAO.BookReference.BookReferenceDAO
import Model.Context.AppContext
import Service.BookReference.BookReferenceMapper

getBookReference :: String -> AppContextM (Either AppError BookReferenceDTO)
getBookReference shortUuid = heFindBookReferenceByShortUuid shortUuid $ \br -> return . Right . toDTO $ br

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetBookReference shortUuid callback = do
  eitherBookReference <- getBookReference shortUuid
  case eitherBookReference of
    Right actionKey -> callback actionKey
    Left error -> return . Left $ error
