module Api.Resource.KnowledgeModel.KnowledgeModelChangeJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Event.EventDTO ()
import Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO

instance ToJSON KnowledgeModelChangeDTO where
  toJSON KnowledgeModelChangeDTO {..} =
    object
      [ "packageId" .= _knowledgeModelChangeDTOPackageId
      , "events" .= _knowledgeModelChangeDTOEvents
      , "tagUuids" .= _knowledgeModelChangeDTOTagUuids
      ]

instance FromJSON KnowledgeModelChangeDTO where
  parseJSON (Object o) = do
    _knowledgeModelChangeDTOPackageId <- o .: "packageId"
    _knowledgeModelChangeDTOEvents <- o .: "events"
    _knowledgeModelChangeDTOTagUuids <- o .: "tagUuids"
    return KnowledgeModelChangeDTO {..}
  parseJSON _ = mzero
