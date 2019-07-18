module Database.BSON.Questionnaire.QuestionnaireLabel where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Model.Questionnaire.QuestionnaireLabel

instance FromBSON Label where
  fromBSON doc = do
    _labelPath <- BSON.lookup "path" doc
    _labelValue <- BSON.lookup "value" doc
    return Label {..}

instance ToBSON Label where
  toBSON Label {..} = ["path" BSON.=: _labelPath, "value" BSON.=: _labelValue]
