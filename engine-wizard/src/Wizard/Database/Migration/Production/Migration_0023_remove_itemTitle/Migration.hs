module Wizard.Database.Migration.Production.Migration_0023_remove_itemTitle.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)
import Prelude hiding (lookup)

definition = (meta, migrate)

meta =
  MigrationMeta
    {mmNumber = 23, mmName = "Remove item title", mmDescription = "Rename itemName from questionnaire's replies"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool
  -- 1. Find
 = do
  let findAction = rest =<< find (select [] "questionnaires")
  qtns <- runMongoDBPoolDef findAction dbPool
  -- 2. Update
  let updateQtns = fmap sanitizeQuestionnaire qtns
  -- 3. Delete old
  let deleteAction = delete $ select [] "questionnaires"
  runMongoDBPoolDef deleteAction dbPool
  -- 4. Insert update
  let insertAction = insertMany "questionnaires" updateQtns
  runMongoDBPoolDef insertAction dbPool
  return Nothing

sanitizeQuestionnaire :: [Field] -> [Field]
sanitizeQuestionnaire = fmap sanitizeReplies

sanitizeReplies :: Field -> Field
sanitizeReplies field =
  case label field of
    "replies" ->
      case value field of
        (Array replies) -> field {value = Array . catMaybes . fmap sanitizeReply $ replies}
        _ -> field
    _ -> field

sanitizeReply :: Value -> Maybe Value
sanitizeReply (Doc doc) =
  case lookup "path" doc of
    Just path ->
      if T.isInfixOf "itemName" path
        then Nothing
        else Just . Doc $ doc
    Nothing -> Just . Doc $ doc
sanitizeReply value = Just value
