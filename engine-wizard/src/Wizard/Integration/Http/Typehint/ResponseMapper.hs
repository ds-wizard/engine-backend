module Wizard.Integration.Http.Typehint.ResponseMapper
  ( toRetrieveTypehintsResponse
  ) where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either (rights)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.Wreq (Response)
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.String (splitOn)
import Wizard.Integration.Http.Common.ResponseMapper
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Util.Template

toRetrieveTypehintsResponse :: ApiIntegration -> Response BSL.ByteString -> Either String [TypehintIDTO]
toRetrieveTypehintsResponse intConfig response =
  case extractResponseBody response >>= extractNestedField listField >>= convertToArray >>= mapRecords of
    Right dto -> Right dto
    Left error -> Left . show $ error
  where
    listField = splitOn "." (intConfig ^. responseListField)
    mapRecords :: [Value] -> Either AppError [TypehintIDTO]
    mapRecords = Right . rights . fmap mapRecord
    mapRecord :: Value -> Either String TypehintIDTO
    mapRecord record = do
      let contextMap = HM.fromList [("item", record)]
      itemId <- renderEither' (intConfig ^. responseItemId) contextMap
      itemTemplate <- renderEither' (intConfig ^. responseItemTemplate) contextMap
      Right $ TypehintIDTO {_typehintIDTOIntId = T.unpack itemId, _typehintIDTOName = T.unpack itemTemplate}
