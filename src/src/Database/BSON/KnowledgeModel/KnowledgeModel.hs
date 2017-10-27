module Database.BSON.KnowledgeModel.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Model.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ToBSON KnowledgeModel where
  toBSON km =
    [ "uuid" BSON.=: toString (km ^. kmUuid)
    , "name" BSON.=: (km ^. kmName)
    , "chapters" BSON.=: (km ^. kmChapters)
    ]

instance FromBSON KnowledgeModel where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    name <- BSON.lookup "name" doc
    chapters <- BSON.lookup "chapters" doc
    return
      KnowledgeModel {_kmUuid = uuid, _kmName = name, _kmChapters = chapters}

-- -------------------------
-- CHAPTER -----------------
-- -------------------------
instance ToBSON Chapter where
  toBSON user =
    [ "uuid" BSON.=: toString (user ^. chUuid)
    , "_chNamespace" BSON.=: (user ^. chNamespace)
    , "formatVersion" BSON.=: (user ^. chFormatVersion)
    , "title" BSON.=: (user ^. chTitle)
    , "text" BSON.=: (user ^. chText)
    , "questions" BSON.=: (user ^. chQuestions)
    ]

instance FromBSON Chapter where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    namespace <- BSON.lookup "namespace" doc
    formatVersion <- BSON.lookup "formatVersion" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    questions <- BSON.lookup "questions" doc
    return
      Chapter
      { _chUuid = uuid
      , _chNamespace = namespace
      , _chFormatVersion = formatVersion
      , _chTitle = title
      , _chText = text
      , _chQuestions = questions
      }

-- -------------------------
-- QUESTION ----------------
-- -------------------------      
instance ToBSON Question where
  toBSON user =
    [ "uuid" BSON.=: toString (user ^. qUuid)
    , "type" BSON.=: (user ^. qType)
    , "title" BSON.=: (user ^. qTitle)
    , "text" BSON.=: (user ^. qText)
    , "answers" BSON.=: (user ^. qAnswers)
    , "references" BSON.=: (user ^. qReferences)
    , "experts" BSON.=: (user ^. qExperts)
    ]

instance FromBSON Question where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    qType <- BSON.lookup "type" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    answers <- BSON.lookup "answers" doc
    references <- BSON.lookup "references" doc
    experts <- BSON.lookup "experts" doc
    return
      Question
      { _qUuid = uuid
      , _qType = qType
      , _qTitle = title
      , _qText = text
      , _qAnswers = answers
      , _qReferences = references
      , _qExperts = experts
      }

-- -------------------------
-- ANSWER ------------------
-- -------------------------      
instance ToBSON Answer where
  toBSON user =
    [ "uuid" BSON.=: toString (user ^. ansUuid)
    , "label" BSON.=: (user ^. ansLabel)
    , "advice" BSON.=: (user ^. ansAdvice)
    , "following" BSON.=: (user ^. ansFollowing)
    ]

instance FromBSON Answer where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    label <- BSON.lookup "label" doc
    advice <- BSON.lookup "advice" doc
    following <- BSON.lookup "following" doc
    return
      Answer
      { _ansUuid = uuid
      , _ansLabel = label
      , _ansAdvice = advice
      , _ansFollowing = following
      }

-- -------------------------
-- EXPERT ------------------
-- -------------------------
instance ToBSON Expert where
  toBSON user =
    [ "uuid" BSON.=: toString (user ^. expUuid)
    , "name" BSON.=: (user ^. expName)
    , "email" BSON.=: (user ^. expEmail)
    ]

instance FromBSON Expert where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    name <- BSON.lookup "name" doc
    email <- BSON.lookup "email" doc
    return Expert {_expUuid = uuid, _expName = name, _expEmail = email}

-- -------------------------
-- REFERENCE ---------------
-- -------------------------
instance ToBSON Reference where
  toBSON user =
    [ "uuid" BSON.=: toString (user ^. refUuid)
    , "chapter" BSON.=: (user ^. refChapter)
    ]

instance FromBSON Reference where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    chapter <- BSON.lookup "chapter" doc
    return Reference {_refUuid = uuid, _refChapter = chapter}
