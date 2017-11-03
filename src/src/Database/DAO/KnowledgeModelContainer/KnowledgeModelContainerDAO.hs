module Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insert, fetch, save, merge, delete,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Types
import Context
import Database.BSON.KnowledgeModelContainer.KnowledgeModelContainer
import Database.DAO.Common
import Model.KnowledgeModelContainer.KnowledgeModelContainer

kmcCollection = "knowledgeModelContainers"

findKnowledgeModelContainers :: Context -> IO [KnowledgeModelContainer]
findKnowledgeModelContainers context = do
 let action = rest =<< find (select [] kmcCollection)
 kmcs <- runMongoDBPoolDef action (context ^. ctxDbPool)
 return $ fmap (fromJust . fromBSON) kmcs

findKnowledgeModelContainerById :: Context -> String -> IO (Maybe KnowledgeModelContainer)
findKnowledgeModelContainerById context kmcUuid = do
 let action = findOne $ select ["uuid" =: kmcUuid] kmcCollection
 maybeKnowledgeModelContainer <- runMongoDBPoolDef action (context ^. ctxDbPool)
 case maybeKnowledgeModelContainer of
   Just kmc -> return . fromBSON $ kmc
   Nothing -> return Nothing

insertKnowledgeModelContainer :: Context -> KnowledgeModelContainer -> IO Value
insertKnowledgeModelContainer context kmc = do
  let action = insert kmcCollection (toBSON kmc)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateKnowledgeModelContainerById :: Context -> KnowledgeModelContainer -> IO ()
updateKnowledgeModelContainerById context kmc = do
 let action =
       fetch (select ["uuid" =: (kmc ^. kmcKmContainerUuid)] kmcCollection) >>=
       save kmcCollection . merge (toBSON kmc)
 runMongoDBPoolDef action (context ^. ctxDbPool)

deleteKnowledgeModelContainers :: Context -> IO ()
deleteKnowledgeModelContainers context = do
 let action = delete $ select [] kmcCollection
 runMongoDBPoolDef action (context ^. ctxDbPool)

deleteKnowledgeModelContainerById :: Context -> String -> IO ()
deleteKnowledgeModelContainerById context kmcUuid = do
 let action = deleteOne $ select ["uuid" =: kmcUuid] kmcCollection
 runMongoDBPoolDef action (context ^. ctxDbPool)
