module Wizard.Database.Mapping.Project.ProjectEvent where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Generics

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Project.Event.ProjectEventJM ()
import Wizard.Database.Mapping.Project.ProjectSharing ()
import Wizard.Database.Mapping.Project.ProjectVisibility ()
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.ProjectReply

instance ToRow ProjectEvent where
  toRow (SetReplyEvent' SetReplyEvent {..}) =
    let values =
          case value of
            StringReply {..} ->
              [ toField StringReplyType
              , toField . PGArray $ [sValue]
              , toField (Nothing :: Maybe String)
              , toField (Nothing :: Maybe String)
              ]
            AnswerReply {..} ->
              [ toField AnswerReplyType
              , toField . PGArray $ [aValue]
              , toField (Nothing :: Maybe String)
              , toField (Nothing :: Maybe String)
              ]
            MultiChoiceReply {..} ->
              [ toField MultiChoiceReplyType
              , toField . PGArray $ mcValue
              , toField (Nothing :: Maybe String)
              , toField (Nothing :: Maybe String)
              ]
            ItemListReply {..} ->
              [ toField ItemListReplyType
              , toField . PGArray $ ilValue
              , toField (Nothing :: Maybe String)
              , toField (Nothing :: Maybe String)
              ]
            IntegrationReply {..} ->
              case iValue of
                PlainType {..} ->
                  [ toField IntegrationReplyType
                  , toField . PGArray $ [value]
                  , toField (Nothing :: Maybe String)
                  , toField (Nothing :: Maybe String)
                  ]
                IntegrationLegacyType {..} ->
                  [ toField IntegrationReplyType
                  , toField . PGArray $ [value]
                  , case intId of
                      Just iId -> toField iId
                      Nothing -> toField "<<integration-type-empty-id>>"
                  , toField (Nothing :: Maybe String)
                  ]
                IntegrationType {..} ->
                  [ toField IntegrationReplyType
                  , toField . PGArray $ [value]
                  , toField (Nothing :: Maybe String)
                  , toField raw
                  ]
            ItemSelectReply {..} ->
              [ toField ItemSelectReplyType
              , toField . PGArray $ [isValue]
              , toField (Nothing :: Maybe String)
              , toField (Nothing :: Maybe String)
              ]
            FileReply {..} ->
              [ toField FileReplyType
              , toField . PGArray $ [fValue]
              , toField (Nothing :: Maybe String)
              , toField (Nothing :: Maybe String)
              ]
     in [ toField uuid
        , toField SetReplyEventType
        , toField path
        , toField createdAt
        , toField createdBy
        , toField projectUuid
        , toField tenantUuid
        ]
          ++ values
  toRow (ClearReplyEvent' ClearReplyEvent {..}) =
    [ toField uuid
    , toField ClearReplyEventType
    , toField path
    , toField createdAt
    , toField createdBy
    , toField projectUuid
    , toField tenantUuid
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    ]
  toRow (SetPhaseEvent' SetPhaseEvent {..}) =
    [ toField uuid
    , toField SetPhaseEventType
    , toField (Nothing :: Maybe String)
    , toField createdAt
    , toField createdBy
    , toField projectUuid
    , toField tenantUuid
    , toField (Nothing :: Maybe String)
    , case phaseUuid of
        Just pUuid -> toField . PGArray $ [pUuid]
        Nothing -> toField . PGArray $ ([] :: [U.UUID])
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    ]
  toRow (SetLabelsEvent' SetLabelsEvent {..}) =
    [ toField uuid
    , toField SetLabelsEventType
    , toField path
    , toField createdAt
    , toField createdBy
    , toField projectUuid
    , toField tenantUuid
    , toField (Nothing :: Maybe String)
    , toField . PGArray $ value
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    ]

instance FromRow ProjectEvent where
  fromRow = do
    uuid <- field
    aType <- field
    mPath <- field
    createdAt <- field
    createdBy <- field
    projectUuid <- field
    tenantUuid <- field
    valueType <- field
    valueText <- fieldValueText
    mValueId <- field
    mValueRaw <- field
    case aType of
      SetReplyEventType -> do
        return . SetReplyEvent' $
          SetReplyEvent
            { uuid = uuid
            , path = fromJust mPath
            , value = parseValue valueType valueText mValueId mValueRaw
            , projectUuid = projectUuid
            , tenantUuid = tenantUuid
            , createdBy = createdBy
            , createdAt = createdAt
            }
      ClearReplyEventType ->
        let path = fromJust mPath
         in return . ClearReplyEvent' $ ClearReplyEvent {..}
      SetPhaseEventType ->
        return . SetPhaseEvent' $
          SetPhaseEvent
            { uuid = uuid
            , phaseUuid =
                case valueText of
                  [value] -> Just . u' $ value
                  _ -> Nothing
            , projectUuid = projectUuid
            , tenantUuid = tenantUuid
            , createdBy = createdBy
            , createdAt = createdAt
            }
      SetLabelsEventType ->
        return . SetLabelsEvent' $
          SetLabelsEvent
            { uuid = uuid
            , path = fromJust mPath
            , value = fmap u' valueText
            , projectUuid = projectUuid
            , tenantUuid = tenantUuid
            , createdBy = createdBy
            , createdAt = createdAt
            }

parseValue valueType valueText mValueId mValueRaw =
  case valueType of
    Just StringReplyType -> StringReply . head $ valueText
    Just AnswerReplyType -> AnswerReply . u' . head $ valueText
    Just MultiChoiceReplyType -> MultiChoiceReply . fmap u' $ valueText
    Just ItemListReplyType -> ItemListReply . fmap u' $ valueText
    Just IntegrationReplyType ->
      IntegrationReply
        { iValue =
            case mValueRaw of
              Just raw -> IntegrationType {value = head valueText, raw = raw}
              Nothing ->
                case mValueId of
                  Just "<<integration-type-empty-id>>" -> IntegrationLegacyType {intId = Nothing, value = head valueText}
                  Just valueId -> IntegrationLegacyType {intId = Just valueId, value = head valueText}
                  Nothing -> PlainType {value = head valueText}
        }
    Just ItemSelectReplyType -> ItemSelectReply . u' . head $ valueText
    Just FileReplyType -> FileReply . u' . head $ valueText
    _ -> error $ "Unknown value type: " ++ show valueType

parseValueText :: Maybe (PGArray T.Text) -> [String]
parseValueText mValueText =
  case mValueText :: Maybe (PGArray T.Text) of
    Just valueText -> fmap T.unpack . fromPGArray $ valueText
    Nothing -> []

fieldValueText :: RowParser [String]
fieldValueText = do
  mValueText <- field
  return $ parseValueText mValueText

parsePhaseUuid :: [String] -> Maybe U.UUID
parsePhaseUuid valueText =
  case valueText of
    [value] -> Just . u' $ value
    _ -> Nothing

data ProjectEventType
  = SetReplyEventType
  | ClearReplyEventType
  | SetPhaseEventType
  | SetLabelsEventType
  deriving (Show, Read, Eq, Ord, Generic)

instance FromField ProjectEventType where
  fromField f mdata = do
    typename <- typename f -- Get the PostgreSQL type name
    case mdata of
      Just bs | typename == "project_event_type" ->
        case bs of
          "SetReplyEvent" -> pure SetReplyEventType
          "ClearReplyEvent" -> pure ClearReplyEventType
          "SetPhaseEvent" -> pure SetPhaseEventType
          "SetLabelsEvent" -> pure SetLabelsEventType
          _ -> returnError ConversionFailed f "Invalid ENUM value"
      _ -> returnError Incompatible f "Expected project_event_type ENUM"

instance ToField ProjectEventType where
  toField SetReplyEventType = toField ("SetReplyEvent" :: T.Text)
  toField ClearReplyEventType = toField ("ClearReplyEvent" :: T.Text)
  toField SetPhaseEventType = toField ("SetPhaseEvent" :: T.Text)
  toField SetLabelsEventType = toField ("SetLabelsEvent" :: T.Text)

data ProjectReplyType
  = StringReplyType
  | AnswerReplyType
  | MultiChoiceReplyType
  | ItemListReplyType
  | IntegrationReplyType
  | ItemSelectReplyType
  | FileReplyType
  deriving (Show, Read, Eq, Ord, Generic)

instance FromField ProjectReplyType where
  fromField f mdata = do
    typename <- typename f -- Get the PostgreSQL type name
    case mdata of
      Just bs ->
        case bs of
          "StringReply" -> pure StringReplyType
          "AnswerReply" -> pure AnswerReplyType
          "MultiChoiceReply" -> pure MultiChoiceReplyType
          "ItemListReply" -> pure ItemListReplyType
          "IntegrationReply" -> pure IntegrationReplyType
          "ItemSelectReply" -> pure ItemSelectReplyType
          "FileReply" -> pure FileReplyType
          _ -> returnError ConversionFailed f "Invalid ENUM value"
      _ -> returnError Incompatible f "Expected value_type ENUM"

instance ToField ProjectReplyType where
  toField StringReplyType = toField ("StringReply" :: T.Text)
  toField AnswerReplyType = toField ("AnswerReply" :: T.Text)
  toField MultiChoiceReplyType = toField ("MultiChoiceReply" :: T.Text)
  toField ItemListReplyType = toField ("ItemListReply" :: T.Text)
  toField IntegrationReplyType = toField ("IntegrationReply" :: T.Text)
  toField ItemSelectReplyType = toField ("ItemSelectReply" :: T.Text)
  toField FileReplyType = toField ("FileReply" :: T.Text)
