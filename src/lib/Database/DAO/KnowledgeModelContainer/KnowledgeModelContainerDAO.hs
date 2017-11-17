module Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insert, fetch, save, merge, delete,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Error
import Common.Types
import Common.Context
import Database.BSON.KnowledgeModelContainer.KnowledgeModelContainer
import Database.BSON.KnowledgeModelContainer.KnowledgeModelContainerWithEvents
import Database.DAO.Common
import Model.KnowledgeModelContainer.KnowledgeModelContainer

kmcCollection = "knowledgeModelContainers"

findKnowledgeModelContainers :: Context
                             -> IO (Either AppError [KnowledgeModelContainer])
findKnowledgeModelContainers context = do
  let action = rest =<< find (select [] kmcCollection)
  kmcsS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ kmcsS

findKnowledgeModelContainerById :: Context
                                -> String
                                -> IO (Either AppError KnowledgeModelContainer)
findKnowledgeModelContainerById context kmcUuid = do
  let action = findOne $ select ["uuid" =: kmcUuid] kmcCollection
  maybeKmcS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeKmcS

findKnowledgeModelContainerByArtifactId :: Context
                                        -> String
                                        -> IO (Either AppError KnowledgeModelContainer)
findKnowledgeModelContainerByArtifactId context artifactId = do
  let action = findOne $ select ["artifactId" =: artifactId] kmcCollection
  maybeKmcS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeKmcS

findKnowledgeModelContainerWithEventsById :: Context
                                          -> String
                                          -> IO (Either AppError KnowledgeModelContainerWithEvents)
findKnowledgeModelContainerWithEventsById context kmcUuid = do
  let action = findOne $ select ["uuid" =: kmcUuid] kmcCollection
  maybeKmcS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeKmcS

insertKnowledgeModelContainer :: Context -> KnowledgeModelContainer -> IO Value
insertKnowledgeModelContainer context kmc = do
  let action = insert kmcCollection (toBSON kmc)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateKnowledgeModelContainerById :: Context -> KnowledgeModelContainer -> IO ()
updateKnowledgeModelContainerById context kmc = do
  let action =
        fetch (select ["uuid" =: (kmc ^. kmcKmcUuid)] kmcCollection) >>=
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
