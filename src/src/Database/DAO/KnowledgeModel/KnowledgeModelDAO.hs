module Database.DAO.KnowledgeModel.KnowledgeModelDAO where
    
import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB (find, findOne, select, insert, fetch, save, merge, delete, deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Types
import Context
import Database.DAO.Common
import Model.KnowledgeModel
import Database.BSON.KnowledgeModel.KnowledgeModel

kmCollection = "knowledgeModels"

findKnowledgeModels :: Context -> IO [KnowledgeModel]
findKnowledgeModels context = do
    let action = rest =<< find (select [] kmCollection)
    kms <- runMongoDBPoolDef action (context ^. ctxDbPool)
    return $ fmap (fromJust . fromBSON) kms

findKnowledgeModelById :: Context -> String -> IO (Maybe KnowledgeModel)
findKnowledgeModelById context kmUuid = do
    let action = findOne $ select ["uuid" =: kmUuid] kmCollection
    maybeKM <- runMongoDBPoolDef action (context ^. ctxDbPool)
    case maybeKM of
        Just km -> return . fromBSON $ km
        Nothing -> return Nothing  

-- findUserByEmail :: Context -> Email -> IO (Maybe User)
-- findUserByEmail context kmEmail = do
--     let action = findOne $ select ["email" =: kmEmail] kmCollection
--     maybeKM <- runMongoDBPoolDef action (context ^. ctxDbPool)
--     case maybeKM of
--     Just km -> return . fromBSON $ km
--     Nothing -> return Nothing  

-- insertUser :: Context -> User -> IO Value
-- insertUser context km = do
--     let action = insert kmCollection (toBSON km)
--     runMongoDBPoolDef action (context ^. ctxDbPool)

-- updateUserById :: Context -> User -> IO ()
-- updateUserById context km = do
--     let action = fetch (select ["uuid" =: (km ^. uUuid)] kmCollection) >>= save kmCollection . merge (toBSON km)
--     runMongoDBPoolDef action (context ^. ctxDbPool)

-- deleteUsers :: Context -> IO ()
-- deleteUsers context = do
--     let action = delete $ select [] kmCollection
--     runMongoDBPoolDef action (context ^. ctxDbPool)
    
-- deleteUserById :: Context -> String -> IO ()
-- deleteUserById context kmUuid = do
--     let action = deleteOne $ select ["uuid" =: kmUuid] kmCollection
--     runMongoDBPoolDef action (context ^. ctxDbPool)
    