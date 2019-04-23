module Integration.Http.Typehint.ResponseMapper
  ( toRetrieveTypehintsResponse
  ) where

import Control.Applicative ((<*>))
import Control.Lens ((^.))
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Integration.Http.Common.ResponseMapper
import Integration.Resource.Typehint.TypehintIDTO
import LensesConfig
import Localization
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel
import Util.List (foldMaybe)
import Util.String (splitOn)

toRetrieveTypehintsResponse :: Integration -> Response BSL.ByteString -> Either AppError [TypehintIDTO]
toRetrieveTypehintsResponse intConfig response =
  extractResponseBody response >>= extractNestedField (splitOn "." lField) >>= convertToArray >>= mapRecords
  where
    lField = intConfig ^. responseListField
    iField = intConfig ^. responseIdField
    nField = intConfig ^. responseNameField
    mapRecords :: [Value] -> Either AppError [TypehintIDTO]
    mapRecords value =
      case foldMaybe . fmap mapRecord $ value of
        Just records -> Right records
        Nothing -> Left . GeneralServerError $ _ERROR_INTEGRATION_TYPEHINT__RDF_UNABLE_TO_MAP_ID_AND_NAME
    mapRecord :: Value -> Maybe TypehintIDTO
    mapRecord record = TypehintIDTO <$> (extractStringField iField record) <*> (extractStringField nField record)
