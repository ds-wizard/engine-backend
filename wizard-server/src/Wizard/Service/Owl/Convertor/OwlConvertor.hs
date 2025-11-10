module Wizard.Service.Owl.Convertor.OwlConvertor where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (liftIO)
import Data.RDF
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Util.Rdf

_MAP_ENTRY_RDF_TYPE :: String
_MAP_ENTRY_RDF_TYPE = "rdfType"

_MAP_ENTRY_RDF_RELATION :: String
_MAP_ENTRY_RDF_RELATION = "rdfRelation"

convertOwlToEvents :: MonadIO m => T.Text -> T.Text -> m [KnowledgeModelEvent]
convertOwlToEvents rootElement text = do
  let result = parseString (TurtleParser Nothing Nothing) text :: Either ParseFailure (RDF TList)
  case result of
    Left err -> error "Unable to parse RDF content"
    Right graph -> do
      let rootClass = resolveClass graph rootElement
      convertToKnowledgeModel rootClass

convertToKnowledgeModel :: MonadIO m => RdfClass -> m [KnowledgeModelEvent]
convertToKnowledgeModel rdfClass = do
  uuid <- liftIO generateUuid
  entityUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  events <- convertToChapter entityUuid rdfClass
  let entityEvent =
        KnowledgeModelEvent
          { uuid = uuid
          , parentUuid = U.nil
          , entityUuid = entityUuid
          , content =
              AddKnowledgeModelEvent' $
                AddKnowledgeModelEvent
                  { annotations = []
                  }
          , createdAt = now
          }
  return $ entityEvent : events

convertToChapter :: MonadIO m => U.UUID -> RdfClass -> m [KnowledgeModelEvent]
convertToChapter parentUuid rdfClass = do
  uuid <- liftIO generateUuid
  entityUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  events <- convertToQuestion entityUuid rdfClass
  let entityEvent =
        KnowledgeModelEvent
          { uuid = uuid
          , parentUuid = parentUuid
          , entityUuid = entityUuid
          , content =
              AddChapterEvent' $
                AddChapterEvent
                  { title = "Chapter"
                  , text = Nothing
                  , annotations = []
                  }
          , createdAt = now
          }
  return $ entityEvent : events

convertToQuestion :: MonadIO m => U.UUID -> RdfClass -> m [KnowledgeModelEvent]
convertToQuestion parentUuid (RdfClass nameT mCommentT dataTypes objects) = do
  uuid <- liftIO generateUuid
  entityUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let name = T.unpack nameT
  let comment =
        case mCommentT of
          Just commentT ->
            f'
              "\n\
              \##### Description:\n\
              \%s"
              [T.unpack commentT]
          Nothing -> ""
  let entityEvent =
        KnowledgeModelEvent
          { uuid = uuid
          , parentUuid = parentUuid
          , entityUuid = entityUuid
          , content =
              AddQuestionEvent' $
                AddListQuestionEvent' $
                  AddListQuestionEvent
                    { title = getTitle nameT
                    , text =
                        Just $
                          f'
                            "##### RDF:\n\
                            \- **Type:** `%s`%s"
                            [name, comment]
                    , requiredPhaseUuid = Nothing
                    , annotations = [MapEntry _MAP_ENTRY_RDF_TYPE name]
                    , tagUuids = []
                    }
          , createdAt = now
          }
  dataTypeEvents <- traverse (convertDataTypeToEvent entityUuid) dataTypes
  objectEvents <- traverse (convertObjectToEvent entityUuid) objects
  return $ [entityEvent] ++ dataTypeEvents ++ concat objectEvents

convertDataTypeToEvent :: MonadIO m => U.UUID -> RdfDataType -> m KnowledgeModelEvent
convertDataTypeToEvent parentUuid (RdfDataType nameT mCommentT rdfType) = do
  uuid <- liftIO generateUuid
  entityUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let name = T.unpack nameT
  let comment =
        case mCommentT of
          Just commentT ->
            f'
              "\n\
              \##### Description:\n\
              \%s"
              [T.unpack commentT]
          Nothing -> ""
  let valueType =
        case rdfType of
          "http://www.w3.org/2001/XMLSchema#anyURI" -> UrlQuestionValueType
          "http://www.w3.org/2001/XMLSchema#date" -> DateQuestionValueType
          "http://www.w3.org/2001/XMLSchema#dateTime" -> DateTimeQuestionValueType
          "http://www.w3.org/2001/XMLSchema#time" -> TimeQuestionValueType
          "http://www.w3.org/2001/XMLSchema#decimal" -> NumberQuestionValueType
          "http://www.w3.org/2001/XMLSchema#double" -> NumberQuestionValueType
          "http://www.w3.org/2001/XMLSchema#float" -> NumberQuestionValueType
          "http://www.w3.org/2001/XMLSchema#integer" -> NumberQuestionValueType
          "http://www.w3.org/2001/XMLSchema#string" -> StringQuestionValueType
          _ -> StringQuestionValueType
  return $
    KnowledgeModelEvent
      { uuid = uuid
      , parentUuid = parentUuid
      , entityUuid = entityUuid
      , content =
          AddQuestionEvent' $
            AddValueQuestionEvent' $
              AddValueQuestionEvent
                { title = getTitle nameT
                , text =
                    Just $
                      f'
                        "##### RDF:\n\
                        \- **Type:** `%s`%s"
                        [name, comment]
                , requiredPhaseUuid = Nothing
                , tagUuids = []
                , annotations = [MapEntry _MAP_ENTRY_RDF_TYPE name]
                , valueType = valueType
                , validations = []
                }
      , createdAt = now
      }

convertObjectToEvent :: MonadIO m => U.UUID -> RdfObject -> m [KnowledgeModelEvent]
convertObjectToEvent parentUuid (RdfObject objName mCommentT (RdfClass nameT mClassCommentT dataTypes objects)) = do
  uuid <- liftIO generateUuid
  entityUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let name = T.unpack nameT
  let comment =
        case mCommentT of
          Just commentT ->
            f'
              "\n\
              \##### Description:\n\
              \%s"
              [T.unpack commentT]
          Nothing -> ""
  let classComment =
        case mClassCommentT of
          Just classCommentT ->
            f'
              "\n\
              \##### Relation Description:\n\
              \%s"
              [T.unpack classCommentT]
          Nothing -> ""
  let entityEvent =
        KnowledgeModelEvent
          { uuid = uuid
          , parentUuid = parentUuid
          , entityUuid = entityUuid
          , content =
              AddQuestionEvent' $
                AddListQuestionEvent' $
                  AddListQuestionEvent
                    { title = getTitle nameT
                    , text =
                        Just $
                          f'
                            "##### RDF:\n\
                            \- **Type:** `%s`\n\
                            \- **Relation:** `%s`%s%s"
                            [name, T.unpack objName, comment, classComment]
                    , requiredPhaseUuid = Nothing
                    , annotations =
                        [ MapEntry _MAP_ENTRY_RDF_TYPE name
                        , MapEntry "rdfRelation" (T.unpack objName)
                        ]
                    , tagUuids = []
                    }
          , createdAt = now
          }
  dataTypeEvents <- traverse (convertDataTypeToEvent entityUuid) dataTypes
  objectEvents <- traverse (convertObjectToEvent entityUuid) objects
  return $ [entityEvent] ++ dataTypeEvents ++ concat objectEvents

getTitle :: T.Text -> String
getTitle name =
  if '#' `elem` T.unpack name
    then T.unpack . T.toTitle . T.pack . unwords . fromHumps . T.unpack $ T.splitOn "#" name !! 1
    else T.unpack . T.toTitle . T.pack . unwords . fromHumps . T.unpack . last $ T.splitOn "/" name
