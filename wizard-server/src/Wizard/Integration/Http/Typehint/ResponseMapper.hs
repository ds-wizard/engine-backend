module Wizard.Integration.Http.Typehint.ResponseMapper (
  toRetrieveTypehintsResponse,
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either (rights)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Common.Integration.Http.Common.ResponseMapper
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String (splitOn)
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Util.DocumentTemplate
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

toRetrieveTypehintsResponse :: ApiIntegration -> Response BSL.ByteString -> Either String [TypehintIDTO]
toRetrieveTypehintsResponse intConfig response =
  case extractResponseBody response >>= extractNestedField listField >>= convertToArray >>= mapRecords of
    Right dto -> Right dto
    Left error -> Left . show $ error
  where
    listField =
      case intConfig.responseListField of
        Just responseListField -> splitOn "." responseListField
        Nothing -> []
    mapRecords :: [Value] -> Either AppError [TypehintIDTO]
    mapRecords = Right . rights . fmap mapRecord
    mapRecord :: Value -> Either String TypehintIDTO
    mapRecord record = do
      let contextMap = HM.fromList [("item", record)]
      itemId <-
        case intConfig.responseItemId of
          Just responseItemId -> do
            result <- renderEither' responseItemId contextMap
            Right . Just . T.unpack $ result
          Nothing -> Right Nothing
      itemTemplate <- renderEither' intConfig.responseItemTemplate contextMap
      Right $ TypehintIDTO {intId = itemId, name = T.unpack itemTemplate}
