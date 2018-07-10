module Database.BSON.KnowledgeModel.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.UUID

import Database.BSON.Common
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ToBSON KnowledgeModel where
  toBSON km =
    ["uuid" BSON.=: serializeUUID (km ^. uuid), "name" BSON.=: (km ^. name), "chapters" BSON.=: (km ^. chapters)]

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
    [ "uuid" BSON.=: serializeUUID (model ^. uuid)
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
    [ "uuid" BSON.=: serializeUUID (model ^. uuid)
    , "type" BSON.=: show (model ^. qType)
    , "title" BSON.=: (model ^. title)
    , "text" BSON.=: (model ^. text)
    , "answers" BSON.=: (model ^. answers)
    , "answerItemTemplate" BSON.=: (model ^. answerItemTemplate)
    , "references" BSON.=: (model ^. references)
    -- , "references" BSON.=: convertReferenceToBSON <$> (model ^. references)
    , "experts" BSON.=: (model ^. experts)
    ]

instance FromBSON Question where
  fromBSON doc = do
    qUuidS <- BSON.lookup "uuid" doc
    qUuid <- fromString qUuidS
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
    [ "uuid" BSON.=: serializeUUID (model ^. uuid)
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
    ["uuid" BSON.=: serializeUUID (model ^. uuid), "name" BSON.=: (model ^. name), "email" BSON.=: (model ^. email)]

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
  toBSON (ResourcePageReference' event) = toBSON event
  toBSON (URLReference' event) = toBSON event
  toBSON (CrossReference' event) = toBSON event

instance FromBSON Reference where
  fromBSON doc = do
    referenceType <- BSON.lookup "referenceType" doc
    case referenceType of
      "ResourcePageReference" -> ResourcePageReference' <$> (fromBSON doc :: Maybe ResourcePageReference)
      "URLReference" -> URLReference' <$> (fromBSON doc :: Maybe URLReference)
      "CrossReference" -> CrossReference' <$> (fromBSON doc :: Maybe CrossReference)

-- ------------------------------------------------
instance ToBSON ResourcePageReference where
  toBSON model =
    [ "referenceType" BSON.=: "ResourcePageReference"
    , "uuid" BSON.=: serializeUUID (model ^. uuid)
    , "shortUuid" BSON.=: (model ^. shortUuid)
    ]

instance FromBSON ResourcePageReference where
  fromBSON doc = do
    refUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    refShortUuid <- BSON.lookup "shortUuid" doc
    return ResourcePageReference {_resourcePageReferenceUuid = refUuid, _resourcePageReferenceShortUuid = refShortUuid}

-- ------------------------------------------------
instance ToBSON URLReference where
  toBSON model =
    [ "referenceType" BSON.=: "URLReference"
    , "uuid" BSON.=: serializeUUID (model ^. uuid)
    , "url" BSON.=: (model ^. url)
    , "anchor" BSON.=: (model ^. anchor)
    ]

instance FromBSON URLReference where
  fromBSON doc = do
    refUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    refUrl <- BSON.lookup "url" doc
    refAnchor <- BSON.lookup "anchor" doc
    return URLReference {_uRLReferenceUuid = refUuid, _uRLReferenceUrl = refUrl, _uRLReferenceAnchor = refAnchor}

-- ------------------------------------------------
instance ToBSON CrossReference where
  toBSON model =
    [ "referenceType" BSON.=: "CrossReference"
    , "uuid" BSON.=: serializeUUID (model ^. uuid)
    , "targetUuid" BSON.=: serializeUUID (model ^. targetUuid)
    ]

instance FromBSON CrossReference where
  fromBSON doc = do
    refUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    refTargetUuid <- deserializeMaybeUUID $ BSON.lookup "targetUuid" doc
    return CrossReference {_crossReferenceUuid = refUuid, _crossReferenceTargetUuid = refTargetUuid}
