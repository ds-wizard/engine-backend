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
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel
import Util.List (foldEither)
import Util.String (splitOn)

toRetrieveTypehintsResponse :: Integration -> Response BSL.ByteString -> Either AppError [TypehintIDTO]
toRetrieveTypehintsResponse intConfig response =
  extractResponseBody response >>= extractNestedField lField >>= convertToArray >>= mapRecords
  where
    lField = splitOn "." (intConfig ^. responseListField)
    iField = splitOn "." (intConfig ^. responseIdField)
    nField = splitOn "." (intConfig ^. responseNameField)
    mapRecords :: [Value] -> Either AppError [TypehintIDTO]
    mapRecords value = foldEither . fmap mapRecord $ value
    mapRecord :: Value -> Either AppError TypehintIDTO
    mapRecord record =
      TypehintIDTO <$> (extractNestedStringField iField record) <*> (extractNestedStringField nField record)
