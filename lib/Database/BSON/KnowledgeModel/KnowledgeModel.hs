module Database.BSON.KnowledgeModel.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ToBSON KnowledgeModel where
  toBSON km = ["uuid" BSON.=: toString (km ^. uuid), "name" BSON.=: (km ^. name), "chapters" BSON.=: (km ^. chapters)]

instance FromBSON KnowledgeModel where
  fromBSON doc = do
    kmUuidS <- BSON.lookup "uuid" doc
    kmUuid <- fromString kmUuidS
    kmName <- BSON.lookup "name" doc
    kmChapters <- BSON.lookup "chapters" doc
    return
      KnowledgeModel {_knowledgeModelUuid = kmUuid, _knowledgeModelName = kmName, _knowledgeModelChapters = kmChapters}

-- -------------------------
-- CHAPTER -----------------
-- -------------------------
instance ToBSON Chapter where
  toBSON model =
    [ "uuid" BSON.=: toString (model ^. uuid)
    , "title" BSON.=: (model ^. title)
    , "text" BSON.=: (model ^. text)
    , "questions" BSON.=: (model ^. questions)
    ]

instance FromBSON Chapter where
  fromBSON doc = do
    chUuidS <- BSON.lookup "uuid" doc
    chUuid <- fromString chUuidS
    chTitle <- BSON.lookup "title" doc
    chText <- BSON.lookup "text" doc
    chQuestions <- BSON.lookup "questions" doc
    return
      Chapter {_chapterUuid = chUuid, _chapterTitle = chTitle, _chapterText = chText, _chapterQuestions = chQuestions}

-- -------------------------
-- QUESTION ----------------
-- -------------------------
instance ToBSON Question where
  toBSON model =
    [ "uuid" BSON.=: toString (model ^. uuid)
    , "shortUuid" BSON.=: (model ^. shortUuid)
    , "type" BSON.=: show (model ^. qType)
    , "title" BSON.=: (model ^. title)
    , "text" BSON.=: (model ^. text)
    , "answers" BSON.=: (model ^. answers)
    , "answerItemTemplate" BSON.=: (model ^. answerItemTemplate)
    , "references" BSON.=: (model ^. references)
    , "experts" BSON.=: (model ^. experts)
    ]

instance FromBSON Question where
  fromBSON doc = do
    qUuidS <- BSON.lookup "uuid" doc
    qUuid <- fromString qUuidS
    qShortUuid <- BSON.lookup "shortUuid" doc
    qQType <- deserializeQuestionType $ BSON.lookup "type" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qAnswers <- BSON.lookup "answers" doc
    qAnswerItemTemplate <- BSON.lookup "answerItemTemplate" doc
    qReferences <- BSON.lookup "references" doc
    qExperts <- BSON.lookup "experts" doc
    return
      Question
      { _questionUuid = qUuid
      , _questionShortUuid = qShortUuid
      , _questionQType = qQType
      , _questionTitle = qTitle
      , _questionText = qText
      , _questionAnswers = qAnswers
      , _questionAnswerItemTemplate = qAnswerItemTemplate
      , _questionReferences = qReferences
      , _questionExperts = qExperts
      }

-- -------------------------
-- ANSWER ------------------
-- -------------------------
instance ToBSON Answer where
  toBSON model =
    [ "uuid" BSON.=: toString (model ^. uuid)
    , "label" BSON.=: (model ^. label)
    , "advice" BSON.=: (model ^. advice)
    , "followUps" BSON.=: (model ^. followUps)
    ]

instance FromBSON Answer where
  fromBSON doc = do
    ansUuidS <- BSON.lookup "uuid" doc
    ansUuid <- fromString ansUuidS
    ansLabel <- BSON.lookup "label" doc
    ansAdvice <- BSON.lookup "advice" doc
    ansFollowUps <- BSON.lookup "followUps" doc
    return
      Answer
      {_answerUuid = ansUuid, _answerLabel = ansLabel, _answerAdvice = ansAdvice, _answerFollowUps = ansFollowUps}

-- -------------------------
-- ANSWER ITEM TEMPLATE ----
-- -------------------------
instance ToBSON AnswerItemTemplate where
  toBSON model = ["title" BSON.=: model ^. title, "questions" BSON.=: (model ^. questions)]

instance FromBSON AnswerItemTemplate where
  fromBSON doc = do
    aitTitle <- BSON.lookup "title" doc
    aitQuestions <- BSON.lookup "questions" doc
    return AnswerItemTemplate {_answerItemTemplateTitle = aitTitle, _answerItemTemplateQuestions = aitQuestions}

instance ToBSON AnswerItemTemplatePlain where
  toBSON model = ["title" BSON.=: model ^. title]

instance FromBSON AnswerItemTemplatePlain where
  fromBSON doc = do
    aitTitle <- BSON.lookup "title" doc
    return AnswerItemTemplatePlain {_answerItemTemplatePlainTitle = aitTitle}

instance ToBSON AnswerItemTemplatePlainWithIds where
  toBSON model = ["title" BSON.=: model ^. title, "questionsIds" BSON.=: serializeUUIDList (model ^. questionIds)]

instance FromBSON AnswerItemTemplatePlainWithIds where
  fromBSON doc = do
    aitTitle <- BSON.lookup "title" doc
    aitQuestionIds <- deserializeUUIDList $ BSON.lookup "questions" doc
    return
      AnswerItemTemplatePlainWithIds
      {_answerItemTemplatePlainWithIdsTitle = aitTitle, _answerItemTemplatePlainWithIdsQuestionIds = aitQuestionIds}

-- -------------------------
-- EXPERT ------------------
-- -------------------------
instance ToBSON Expert where
  toBSON model =
    ["uuid" BSON.=: toString (model ^. uuid), "name" BSON.=: (model ^. name), "email" BSON.=: (model ^. email)]

instance FromBSON Expert where
  fromBSON doc = do
    expUuidS <- BSON.lookup "uuid" doc
    expUuid <- fromString expUuidS
    expName <- BSON.lookup "name" doc
    expEmail <- BSON.lookup "email" doc
    return Expert {_expertUuid = expUuid, _expertName = expName, _expertEmail = expEmail}

-- -------------------------
-- REFERENCE ---------------
-- -------------------------
instance ToBSON Reference where
  toBSON model = ["uuid" BSON.=: toString (model ^. uuid), "chapter" BSON.=: (model ^. chapter)]

instance FromBSON Reference where
  fromBSON doc = do
    refUuidS <- BSON.lookup "uuid" doc
    refUuid <- fromString refUuidS
    refChapter <- BSON.lookup "chapter" doc
    return Reference {_referenceUuid = refUuid, _referenceChapter = refChapter}
