module Wizard.Database.Migration.Production.Migration_0037_questionnaireSharingAndCollaboration.Migration
  ( definition
  ) where

import Control.Monad.Logger hiding (logInfo)
import Control.Monad.Reader (liftIO)
import qualified Data.Bson as BSON
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.List as L
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)
import System.Random (StdGen, mkStdGen, random)

import Shared.Util.List
import Shared.Util.Number
import qualified Shared.Util.String as S

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 37, mmName = "Questionnaire Sharing", mmDescription = "Add questionnaire 'sharing'"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addDefaultSharingToQuestionnaires dbPool
  addSharingToAppConfig dbPool
  renameQuestionnaireVisibilityOptions dbPool
  transformRepliesAndLabelsForQuestionnaires dbPool
  return Nothing

-- ------------------------------------------------------------------------------------------------
addDefaultSharingToQuestionnaires dbPool = do
  let action = modify (select [] "questionnaires") ["$set" =: ["sharing" =: "RestrictedQuestionnaire"]]
  runMongoDBPoolDef action dbPool
  return Nothing

-- ------------------------------------------------------------------------------------------------
addSharingToAppConfig dbPool = do
  let action =
        modify
          (select [] "appConfigs")
          [ "$set" =:
            ["questionnaire.questionnaireSharing" =: ["enabled" =: True, "defaultValue" =: "RestrictedQuestionnaire"]]
          ]
  runMongoDBPoolDef action dbPool

-- ------------------------------------------------------------------------------------------------
renameQuestionnaireVisibilityOptions dbPool = do
  renamePublicReadOnlyQuestionnaires dbPool
  renamePublicQuestionnaires dbPool
  renamePublicReadOnlyInAppConfig dbPool
  renamePublicInAppConfig dbPool

renamePublicReadOnlyQuestionnaires dbPool = do
  let action =
        modify
          (select ["visibility" =: "PublicReadOnlyQuestionnaire"] "questionnaires")
          ["$set" =: ["visibility" =: "VisibleViewQuestionnaire"]]
  runMongoDBPoolDef action dbPool
  return Nothing

renamePublicQuestionnaires dbPool = do
  let action =
        modify
          (select ["visibility" =: "PublicQuestionnaire"] "questionnaires")
          ["$set" =: ["visibility" =: "VisibleEditQuestionnaire"]]
  runMongoDBPoolDef action dbPool
  return Nothing

renamePublicReadOnlyInAppConfig dbPool = do
  let action =
        modify
          (select ["questionnaire.questionnaireVisibility.defaultValue" =: "PublicReadOnlyQuestionnaire"] "appConfigs")
          ["$set" =: ["questionnaire.questionnaireVisibility.defaultValue" =: "VisibleViewQuestionnaire"]]
  runMongoDBPoolDef action dbPool

renamePublicInAppConfig dbPool = do
  let action =
        modify
          (select ["questionnaire.questionnaireVisibility.defaultValue" =: "PublicQuestionnaire"] "appConfigs")
          ["$set" =: ["questionnaire.questionnaireVisibility.defaultValue" =: "VisibleEditQuestionnaire"]]
  runMongoDBPoolDef action dbPool

-- ------------------------------------------------------------------------------------------------
transformRepliesAndLabelsForQuestionnaires dbPool = do
  let action = rest =<< find (select [] "questionnaires")
  questionnaires <- runMongoDBPoolDef action dbPool
  traverse_ (transformRepliesAndLabelsForQuestionnaire dbPool) questionnaires

-- ---------------------------------------------
transformRepliesAndLabelsForQuestionnaire dbPool qtn = do
  uuidGenerator <- getGenerator
  let qtnUuid = extractUuid qtn
  let allReplies = extractReplies qtn
  let itemListReplies = filter filterItemList allReplies
  let allLabels = extractLabels qtn
  -- 1. Convert list item to new structure
  let (_, paths, updatedItemListReplies) =
        foldl transformItemList (uuidGenerator, [], []) itemListReplies
  let allRepliesWithoutItems = filter (not . filterItemList) allReplies
  let allRepliesWithItems = allRepliesWithoutItems ++ updatedItemListReplies
  let sortedPaths = L.sortBy (\(d1,_,_) (d2,_,_) -> compare d2 d1) paths
  let updatedAllReplies = foldl updatePaths allRepliesWithItems sortedPaths
  let updatedAllLabels = foldl updatePaths allLabels sortedPaths
  -- 2. Convert to Map
  let updatedAllRepliesMap = convertRepliesListToMap updatedAllReplies
  let updatedAllLabelsMap = convertLabelsListToMap updatedAllLabels
  -- 3. Save
  let action =
        modify
          (select [_UUID =: qtnUuid] "questionnaires")
          ["$set" =: [_REPLIES =: updatedAllRepliesMap, _LABELS =: updatedAllLabelsMap]]
  runMongoDBPoolDef action dbPool

filterItemList :: BSON.Document -> Bool
filterItemList reply = (extractType . extractValue $ reply) == _ITEM_LIST_REPLY

transformItemList ::
     (StdGen, [(Int, String, String)], [BSON.Document])
  -> BSON.Document
  -> (StdGen, [(Int, String, String)], [BSON.Document])
transformItemList (uuidGenerator, allPaths, itemListReplies) itemListReply =
  let numberList = generateList . extractValueInt . extractValue $ itemListReply
      itemListPath = extractPathS itemListReply
      (newUuidGenerator, paths, newUuids) =
        foldl (generateUuidsAndPaths itemListPath) (uuidGenerator, [], []) numberList
      newAllPaths = paths ++ allPaths
      newChangedItemListReply = merge [_VALUE =: [_TYPE =: _ITEM_LIST_REPLY, _VALUE =: newUuids]] itemListReply
      newItemListReplies = newChangedItemListReply : itemListReplies
   in (newUuidGenerator, newAllPaths, newItemListReplies)

updatePaths :: [BSON.Document] -> (Int, String, String) -> [BSON.Document]
updatePaths replies (depth, oldPath, newPath) = fmap (updatePath (depth, oldPath, newPath)) replies

updatePath :: (Int, String, String) -> BSON.Document -> BSON.Document
updatePath (_, oldPath, newPath) reply =
  let updatedNewPath = S.replace oldPath newPath (extractPathS reply)
   in BSON.merge [_PATH =: updatedNewPath] reply

generateUuidsAndPaths ::
     String -> (StdGen, [(Int, String, String)], [String]) -> Int -> (StdGen, [(Int, String, String)], [String])
generateUuidsAndPaths itemListPath (uuidGenerator, paths, acc) number =
  let (newUuid, newUuidGenerator) = random uuidGenerator
      depth = length . filter ('-' `notElem`) . S.splitOn "." $ itemListPath
      (oldPath, newPath) = (itemListPath ++ "." ++ show number ++ ".", itemListPath ++ "." ++ U.toString newUuid ++ ".")
   in (newUuidGenerator, (depth, oldPath, newPath) : paths, acc ++ [U.toString newUuid])

replaceItemListReply newItemListReply itemListReply =
  if extractPath newItemListReply == extractPath itemListReply
    then newItemListReply
    else itemListReply

convertRepliesListToMap = foldl go []
  where
    go acc reply = BSON.merge acc [extractPath reply =: extractValue reply]

convertLabelsListToMap = foldl go []
  where
    go acc reply = BSON.merge acc [extractPath reply =: extractValueArray reply]

-- ------------------------------------------------
-- Extractor
-- ------------------------------------------------
extractUuid :: BSON.Document -> String
extractUuid = fromJust . BSON.lookup _UUID

extractReplies :: BSON.Document -> [BSON.Document]
extractReplies = fromJust . BSON.lookup _REPLIES

extractPath :: BSON.Document -> BSON.Label
extractPath = fromJust . BSON.lookup _PATH

extractPathS :: BSON.Document -> String
extractPathS = T.unpack . extractPath

extractType :: BSON.Document -> String
extractType = fromJust . BSON.lookup _TYPE

extractValue :: BSON.Document -> BSON.Document
extractValue = fromJust . BSON.lookup _VALUE

extractValueInt :: BSON.Document -> Int
extractValueInt = fromJust . BSON.lookup _VALUE

extractValueArray :: BSON.Document -> [BSON.Label]
extractValueArray = fromJust . BSON.lookup _VALUE

extractLabels :: BSON.Document -> [BSON.Document]
extractLabels = fromJust . BSON.lookup _LABELS

-- ------------------------------------------------
-- Private
-- ------------------------------------------------
getGenerator = do
  seed <- liftIO $ generateInt 9999999999
  return . mkStdGen $ seed

convertListToMap = foldl go []
  where
    go acc reply = BSON.merge acc [extractPath reply =: extractValue reply]

-- ------------------------------------------------
-- Field names
-- ------------------------------------------------
_REPLIES = "replies"

_UUID = "uuid"

_TYPE = "type"

_PATH = "path"

_VALUE = "value"

_ITEM_LIST_REPLY = "ItemListReply"

_LABELS = "labels"
