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

-- findKnowledgeModelContainers :: Context -> IO [KnowledgeModelContainer]
-- findKnowledgeModelContainers context = do
--   let action = rest =<< find (select [] userCollection)
--   users <- runMongoDBPoolDef action (context ^. ctxDbPool)
--   return $ fmap (fromJust . fromBSON) users

-- findKnowledgeModelContainerById :: Context -> String -> IO (Maybe KnowledgeModelContainer)
-- findKnowledgeModelContainerById context userUuid = do
--   let action = findOne $ select ["uuid" =: userUuid] userCollection
--   maybeKnowledgeModelContainer <- runMongoDBPoolDef action (context ^. ctxDbPool)
--   case maybeKnowledgeModelContainer of
--     Just user -> return . fromBSON $ user
--     Nothing -> return Nothing

-- findKnowledgeModelContainerByEmail :: Context -> Email -> IO (Maybe KnowledgeModelContainer)
-- findKnowledgeModelContainerByEmail context userEmail = do
--   let action = findOne $ select ["email" =: userEmail] userCollection
--   maybeKnowledgeModelContainer <- runMongoDBPoolDef action (context ^. ctxDbPool)
--   case maybeKnowledgeModelContainer of
--     Just user -> return . fromBSON $ user
--     Nothing -> return Nothing

insertKnowledgeModelContainer :: Context -> KnowledgeModelContainer -> IO Value
insertKnowledgeModelContainer context kmc = do
  let action = insert kmcCollection (toBSON kmc)
  runMongoDBPoolDef action (context ^. ctxDbPool)

-- updateKnowledgeModelContainerById :: Context -> KnowledgeModelContainer -> IO ()
-- updateKnowledgeModelContainerById context user = do
--   let action =
--         fetch (select ["uuid" =: (user ^. uUuid)] userCollection) >>=
--         save userCollection . merge (toBSON user)
--   runMongoDBPoolDef action (context ^. ctxDbPool)

-- deleteKnowledgeModelContainers :: Context -> IO ()
-- deleteKnowledgeModelContainers context = do
--   let action = delete $ select [] userCollection
--   runMongoDBPoolDef action (context ^. ctxDbPool)

-- deleteKnowledgeModelContainerById :: Context -> String -> IO ()
-- deleteKnowledgeModelContainerById context userUuid = do
--   let action = deleteOne $ select ["uuid" =: userUuid] userCollection
--   runMongoDBPoolDef action (context ^. ctxDbPool)
