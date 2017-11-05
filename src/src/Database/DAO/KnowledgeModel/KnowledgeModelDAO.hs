module Database.DAO.KnowledgeModel.KnowledgeModelDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insert, modify, fetch, save, merge, delete,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Types
import Context
import Database.BSON.KnowledgeModel.KnowledgeModel
import Database.BSON.KnowledgeModelContainer.KnowledgeModelContainerWithKM
import Database.DAO.Common
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelContainer.KnowledgeModelContainer

findKnowledgeModelByKmcId :: Context
                          -> String
                          -> IO (Maybe KnowledgeModelContainerWithKM)
findKnowledgeModelByKmcId context kmcUuid = do
  let action = findOne $ select ["uuid" =: kmcUuid] kmcCollection
  maybeKM <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybeKM of
    Just km -> return . fromBSON $ km
    Nothing -> return Nothing

updateKnowledgeModelByKmcId :: Context -> String -> KnowledgeModel -> IO ()
updateKnowledgeModelByKmcId context kmcUuid km = do
  let action =
        modify
          (select ["uuid" =: kmcUuid] kmcCollection)
          ["$set" =: ["knowledgeModel" =: (toBSON km)]]
  runMongoDBPoolDef action (context ^. ctxDbPool)
