module Wizard.Integration.Http.Typehint.ResponseMapper
  ( toRetrieveTypehintsResponse
  ) where

import Control.Lens ((^.))
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.List (foldEither)
import Shared.Util.String (splitOn)
import Wizard.Integration.Http.Common.ResponseMapper
import Wizard.Integration.Resource.Typehint.TypehintIDTO

toRetrieveTypehintsResponse :: Integration -> Response BSL.ByteString -> Either AppError [TypehintIDTO]
toRetrieveTypehintsResponse intConfig response =
  extractResponseBody response >>= extractNestedField lField >>= convertToArray >>= mapRecords
  where
    lField = splitOn "." (intConfig ^. responseListField)
    iField = splitOn "." (intConfig ^. responseIdField)
    nField = splitOn "." (intConfig ^. responseNameField)
    mapRecords :: [Value] -> Either AppError [TypehintIDTO]
    mapRecords = foldEither . fmap mapRecord
    mapRecord :: Value -> Either AppError TypehintIDTO
    mapRecord record =
      TypehintIDTO <$> extractNestedStringField iField record <*> extractNestedStringField nField record
