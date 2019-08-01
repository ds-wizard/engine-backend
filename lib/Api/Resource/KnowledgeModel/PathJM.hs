module Api.Resource.KnowledgeModel.PathJM where

import Data.Aeson

import Api.Resource.KnowledgeModel.PathDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PathItemDTO where
  parseJSON = simpleParseJSON "_pathItemDTO"

instance ToJSON PathItemDTO where
  toJSON = simpleToJSON "_pathItemDTO"
