module Database.BSON.Feedback.Feedback where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import LensesConfig
import Model.Feedback.Feedback

instance ToBSON Feedback where
  toBSON feedback =
    [ "uuid" BSON.=: (feedback ^. uuid)
    , "issueId" BSON.=: (feedback ^. issueId)
    , "questionUuid" BSON.=: (feedback ^. questionUuid)
    , "packageId" BSON.=: (feedback ^. packageId)
    , "title" BSON.=: (feedback ^. title)
    , "content" BSON.=: (feedback ^. content)
    , "createdAt" BSON.=: (feedback ^. createdAt)
    , "updatedAt" BSON.=: (feedback ^. updatedAt)
    ]

instance FromBSON Feedback where
  fromBSON doc = do
    fUuid <- BSON.lookup "uuid" doc
    fIssueId <- BSON.lookup "issueId" doc
    fQuestionUuid <- BSON.lookup "questionUuid" doc
    fPackageId <- BSON.lookup "packageId" doc
    fTitle <- BSON.lookup "title" doc
    fContent <- BSON.lookup "content" doc
    fCreatedAt <- BSON.lookup "createdAt" doc
    fUpdatedAt <- BSON.lookup "updatedAt" doc
    return
      Feedback
      { _feedbackUuid = fUuid
      , _feedbackIssueId = fIssueId
      , _feedbackQuestionUuid = fQuestionUuid
      , _feedbackPackageId = fPackageId
      , _feedbackTitle = fTitle
      , _feedbackContent = fContent
      , _feedbackCreatedAt = fCreatedAt
      , _feedbackUpdatedAt = fUpdatedAt
      }
