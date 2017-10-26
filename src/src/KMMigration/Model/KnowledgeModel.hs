module KMMigration.Model.KnowledgeModel where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import GHC.Generics

import KMMigration.Model.Common

data KnowledgeModel = KnowledgeModel
  { _kmUuid :: UUID
  , _kmName :: String
  , _kmChapters :: [Chapter]
  } deriving (Show, Eq, Generic)

data Chapter = Chapter
  { _chUuid :: UUID
  , _chNamespace :: String
  , _chFormatVersion :: Int
  , _chTitle :: String
  , _chText :: String
  , _chQuestions :: [Question]
  } deriving (Show, Eq, Generic)

data Question = Question
  { _qUuid :: UUID
  , _qType :: String
  , _qTitle :: String
  , _qText :: String
  , _qAnswers :: [Answer]
  , _qReferences :: [Reference]
  , _qExperts :: [Expert]
  } deriving (Show, Eq, Generic)

data Answer = Answer
  { _ansUuid :: UUID
  , _ansLabel :: String
  , _ansAdvice :: Maybe String
  , _ansFollowing :: [Question]
  } deriving (Show, Eq, Generic)

data Expert = Expert
  { _expUuid :: UUID
  , _expName :: String
  , _expEmail :: String
  } deriving (Show, Eq, Generic)

data Reference = Reference
  { _refUuid :: UUID
  , _refChapter :: String
  } deriving (Show, Eq, Generic)

makeLenses ''KnowledgeModel

makeLenses ''Chapter

makeLenses ''Question

makeLenses ''Answer

makeLenses ''Expert

makeLenses ''Reference

-- ------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------
kmChapterIds :: KnowledgeModel -> [UUID]
kmChapterIds km = km ^.. kmChapters . traverse . chUuid

kmChangeChapterIdsOrder :: ([Chapter] -> Identity [UUID])
                        -> KnowledgeModel
                        -> Identity KnowledgeModel
kmChangeChapterIdsOrder convert km =
  Identity $ km & kmChapters .~ orderedChapters
  where
    ids :: Identity [UUID]
    ids = convert (km ^. kmChapters)
    orderedChapters :: [Chapter]
    orderedChapters = concatMap getChapterByUuid (runIdentity ids)
    getChapterByUuid :: UUID -> [Chapter]
    getChapterByUuid uuid =
      filter (\x -> x ^. chUuid == uuid) (km ^. kmChapters)

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
chQuestionIds :: Chapter -> [UUID]
chQuestionIds ch = ch ^.. chQuestions . traverse . qUuid

chChangeQuestionIdsOrder :: ([Question] -> Identity [UUID])
                         -> Chapter
                         -> Identity Chapter
chChangeQuestionIdsOrder convert ch =
  Identity $ ch & chQuestions .~ orderedQuestions
  where
    ids :: Identity [UUID]
    ids = convert (ch ^. chQuestions)
    orderedQuestions :: [Question]
    orderedQuestions = concatMap getQuestionByUuid (runIdentity ids)
    getQuestionByUuid :: UUID -> [Question]
    getQuestionByUuid uuid =
      filter (\x -> x ^. qUuid == uuid) (ch ^. chQuestions)

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
qAnwerIds :: Question -> [UUID]
qAnwerIds q = q ^.. qAnswers . traverse . ansUuid

qChangeAnwerIdsOrder :: ([Answer] -> Identity [UUID])
                     -> Question
                     -> Identity Question
qChangeAnwerIdsOrder convert q = Identity $ q & qAnswers .~ orderedAnwers
  where
    ids :: Identity [UUID]
    ids = convert (q ^. qAnswers)
    orderedAnwers :: [Answer]
    orderedAnwers = concatMap getAnswerByUuid (runIdentity ids)
    getAnswerByUuid :: UUID -> [Answer]
    getAnswerByUuid uuid = filter (\x -> x ^. ansUuid == uuid) (q ^. qAnswers)

------------------------------------------------------------------------------------------
ansFollowingIds :: Answer -> [UUID]
ansFollowingIds ans = ans ^.. ansFollowing . traverse . qUuid

ansChangeFollowingIdsOrder :: ([Question] -> Identity [UUID])
                           -> Answer
                           -> Identity Answer
ansChangeFollowingIdsOrder convert ans =
  Identity $ ans & ansFollowing .~ orderedFollowing
  where
    ids :: Identity [UUID]
    ids = convert (ans ^. ansFollowing)
    orderedFollowing :: [Question]
    orderedFollowing = concatMap getFollowingByUuid (runIdentity ids)
    getFollowingByUuid :: UUID -> [Question]
    getFollowingByUuid uuid =
      filter (\x -> x ^. qUuid == uuid) (ans ^. ansFollowing)

------------------------------------------------------------------------------------------
qExpertIds :: Question -> [UUID]
qExpertIds q = q ^.. qExperts . traverse . expUuid

qChangeExpertIdsOrder :: ([Expert] -> Identity [UUID])
                      -> Question
                      -> Identity Question
qChangeExpertIdsOrder convert q = Identity $ q & qExperts .~ orderedExperts
  where
    ids :: Identity [UUID]
    ids = convert (q ^. qExperts)
    orderedExperts :: [Expert]
    orderedExperts = concatMap getExpertByUuid (runIdentity ids)
    getExpertByUuid :: UUID -> [Expert]
    getExpertByUuid uuid = filter (\x -> x ^. expUuid == uuid) (q ^. qExperts)

------------------------------------------------------------------------------------------
qReferenceIds :: Question -> [UUID]
qReferenceIds q = q ^.. qReferences . traverse . refUuid

qChangeReferenceIdsOrder :: ([Reference] -> Identity [UUID])
                         -> Question
                         -> Identity Question
qChangeReferenceIdsOrder convert q =
  Identity $ q & qReferences .~ orderedReferences
  where
    ids :: Identity [UUID]
    ids = convert (q ^. qReferences)
    orderedReferences :: [Reference]
    orderedReferences = concatMap getReferenceByUuid (runIdentity ids)
    getReferenceByUuid :: UUID -> [Reference]
    getReferenceByUuid uuid =
      filter (\x -> x ^. refUuid == uuid) (q ^. qReferences)
