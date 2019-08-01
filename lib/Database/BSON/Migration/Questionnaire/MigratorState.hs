module Database.BSON.Migration.Questionnaire.MigratorState where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Questionnaire.Questionnaire ()
import Model.Migration.Questionnaire.MigratorState

instance ToBSON MigratorState where
  toBSON MigratorState {..} =
    [ "oldQuestionnaireUuid" BSON.=: _migratorStateOldQuestionnaireUuid
    , "newQuestionnaireUuid" BSON.=: _migratorStateNewQuestionnaireUuid
    , "resolvedQuestionUuids" BSON.=: _migratorStateResolvedQuestionUuids
    ]

instance FromBSON MigratorState where
  fromBSON doc = do
    _migratorStateOldQuestionnaireUuid <- BSON.lookup "oldQuestionnaireUuid" doc
    _migratorStateNewQuestionnaireUuid <- BSON.lookup "newQuestionnaireUuid" doc
    _migratorStateResolvedQuestionUuids <- BSON.lookup "resolvedQuestionUuids" doc
    return MigratorState {..}
