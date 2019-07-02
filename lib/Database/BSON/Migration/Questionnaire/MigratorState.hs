module Database.BSON.Migration.Questionnaire.MigratorState where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Questionnaire.Questionnaire ()
import Model.Migration.Questionnaire.MigratorState

instance ToBSON MigratorState where
  toBSON MigratorState {..} =
    [ "oldQuestionnaireUuid" BSON.=: serializeUUID _migratorStateOldQuestionnaireUuid
    , "newQuestionnaireUuid" BSON.=: serializeUUID _migratorStateNewQuestionnaireUuid
    , "resolvedQuestionUuids" BSON.=: serializeUUIDList _migratorStateResolvedQuestionUuids
    ]

instance FromBSON MigratorState where
  fromBSON doc = do
    _migratorStateOldQuestionnaireUuid <- deserializeMaybeUUID $ BSON.lookup "oldQuestionnaireUuid" doc
    _migratorStateNewQuestionnaireUuid <- deserializeMaybeUUID $ BSON.lookup "newQuestionnaireUuid" doc
    _migratorStateResolvedQuestionUuids <- deserializeMaybeUUIDList $ BSON.lookup "resolvedQuestionUuids" doc
    return MigratorState {..}
