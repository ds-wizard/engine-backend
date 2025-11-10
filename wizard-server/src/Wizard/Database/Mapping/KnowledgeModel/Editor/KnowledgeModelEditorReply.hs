module Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorReply where

import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Uuid
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Wizard.Model.Questionnaire.QuestionnaireReply

instance ToRow KnowledgeModelEditorReply where
  toRow KnowledgeModelEditorReply {..} =
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
     in [toField path]
          ++ values
          ++ [ toField knowledgeModelEditorUuid
             , toJSONField createdBy
             , toField tenantUuid
             , toField createdAt
             ]

instance FromRow KnowledgeModelEditorReply where
  fromRow = do
    path <- field
    valueType <- field
    mValueText <- field
    let valueText =
          case mValueText :: Maybe (PGArray T.Text) of
            Just valueText -> fmap T.unpack . fromPGArray $ valueText
            Nothing -> []
    mValueId <- field
    mValueRaw <- field
    let value =
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
    knowledgeModelEditorUuid <- field
    createdBy <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    return $ KnowledgeModelEditorReply {..}
