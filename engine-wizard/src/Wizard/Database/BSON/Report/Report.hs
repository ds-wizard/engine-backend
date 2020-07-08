module Wizard.Database.BSON.Report.Report where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Report.Report

instance FromBSON Report

instance ToBSON Report

-- --------------------------------------------------------------------
instance FromBSON TotalReport

instance ToBSON TotalReport

-- --------------------------------------------------------------------
instance FromBSON ChapterReport

instance ToBSON ChapterReport

-- --------------------------------------------------------------------
instance FromBSON MetricSummary

instance ToBSON MetricSummary

-- --------------------------------------------------------------------
instance ToBSON Indication where
  toBSON (AnsweredIndication' event) = toBSON event
  toBSON (LevelsAnsweredIndication' event) = toBSON event

instance FromBSON Indication where
  fromBSON doc = do
    indicationType <- BSON.lookup "indicationType" doc
    case indicationType of
      "AnsweredIndication" -> AnsweredIndication' <$> (fromBSON doc :: Maybe AnsweredIndication)
      "LevelsAnsweredIndication" -> LevelsAnsweredIndication' <$> (fromBSON doc :: Maybe LevelsAnsweredIndication)

-- --------------------------------------------------------------------
instance ToBSON AnsweredIndication where
  toBSON AnsweredIndication {..} =
    [ "indicationType" BSON.=: "AnsweredIndication"
    , "answeredQuestions" BSON.=: _answeredIndicationAnsweredQuestions
    , "unansweredQuestions" BSON.=: _answeredIndicationUnansweredQuestions
    ]

instance FromBSON AnsweredIndication

-- --------------------------------------------------------------------
instance ToBSON LevelsAnsweredIndication where
  toBSON LevelsAnsweredIndication {..} =
    [ "indicationType" BSON.=: "LevelsAnsweredIndication"
    , "answeredQuestions" BSON.=: _levelsAnsweredIndicationAnsweredQuestions
    , "unansweredQuestions" BSON.=: _levelsAnsweredIndicationUnansweredQuestions
    ]

instance FromBSON LevelsAnsweredIndication
