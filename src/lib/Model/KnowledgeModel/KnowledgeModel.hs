module Model.KnowledgeModel.KnowledgeModel where

import Control.Lens
import Control.Lens.Traversal
import Data.List
import Data.UUID
import GHC.Generics

data KnowledgeModel = KnowledgeModel
  { _kmUuid :: UUID
  , _kmName :: String
  , _kmChapters :: [Chapter]
  } deriving (Show, Eq, Generic)

data Chapter = Chapter
  { _chUuid :: UUID
  , _chTitle :: String
  , _chText :: String
  , _chQuestions :: [Question]
  } deriving (Show, Eq, Generic)

data Question = Question
  { _qUuid :: UUID
  , _qShortUuid :: Maybe String
  , _qType :: String
  , _qTitle :: String
  , _qText :: String
  , _qAnswers :: [Answer]
  , _qExperts :: [Expert]
  , _qReferences :: [Reference]
  } deriving (Show, Eq, Generic)

data Answer = Answer
  { _ansUuid :: UUID
  , _ansLabel :: String
  , _ansAdvice :: Maybe String
  , _ansFollowUps :: [Question]
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

kmChangeChapterIdsOrder :: ([Chapter] -> Identity [UUID]) -> KnowledgeModel -> Identity KnowledgeModel
kmChangeChapterIdsOrder convert km = Identity $ km & kmChapters .~ orderedChapters
  where
    ids :: Identity [UUID]
    ids = convert (km ^. kmChapters)
    orderedChapters :: [Chapter]
    orderedChapters = concatMap getChapterByUuid (runIdentity ids)
    getChapterByUuid :: UUID -> [Chapter]
    getChapterByUuid uuid = filter (\x -> x ^. chUuid == uuid) (km ^. kmChapters)

getAllChapters :: KnowledgeModel -> [Chapter]
getAllChapters km = km ^. kmChapters

isThereAnyChapterWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyChapterWithGivenUuid km uuid = uuid `elem` (getChapterUuid <$> getAllChapters km)
  where
    getChapterUuid chapter = chapter ^. chUuid

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
chQuestionIds :: Chapter -> [UUID]
chQuestionIds ch = ch ^.. chQuestions . traverse . qUuid

chChangeQuestionIdsOrder :: ([Question] -> Identity [UUID]) -> Chapter -> Identity Chapter
chChangeQuestionIdsOrder convert ch = Identity $ ch & chQuestions .~ orderedQuestions
  where
    ids :: Identity [UUID]
    ids = convert (ch ^. chQuestions)
    orderedQuestions :: [Question]
    orderedQuestions = concatMap getQuestionByUuid (runIdentity ids)
    getQuestionByUuid :: UUID -> [Question]
    getQuestionByUuid uuid = filter (\x -> x ^. qUuid == uuid) (ch ^. chQuestions)

getAllQuestions :: KnowledgeModel -> [Question]
getAllQuestions km = go (km ^.. kmChapters . traverse . chQuestions . traverse)
  where
    go :: [Question] -> [Question]
    go [] = []
    go questions = questions ++ (go . concat $ getNestedQuestions <$> questions)
    getNestedQuestions :: Question -> [Question]
    getNestedQuestions question = question ^.. qAnswers . traverse . ansFollowUps . traverse

isThereAnyQuestionWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyQuestionWithGivenUuid km uuid = uuid `elem` (getQuestionUuid <$> getAllQuestions km)
  where
    getQuestionUuid question = question ^. qUuid

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
qAnwerIds :: Question -> [UUID]
qAnwerIds q = q ^.. qAnswers . traverse . ansUuid

qChangeAnwerIdsOrder :: ([Answer] -> Identity [UUID]) -> Question -> Identity Question
qChangeAnwerIdsOrder convert q = Identity $ q & qAnswers .~ orderedAnwers
  where
    ids :: Identity [UUID]
    ids = convert (q ^. qAnswers)
    orderedAnwers :: [Answer]
    orderedAnwers = concatMap getAnswerByUuid (runIdentity ids)
    getAnswerByUuid :: UUID -> [Answer]
    getAnswerByUuid uuid = filter (\x -> x ^. ansUuid == uuid) (q ^. qAnswers)

getAllAnswers :: KnowledgeModel -> [Answer]
getAllAnswers km = concat $ getAnswer <$> getAllQuestions km
  where
    getAnswer :: Question -> [Answer]
    getAnswer question = question ^. qAnswers

isThereAnyAnswerWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyAnswerWithGivenUuid km uuid = uuid `elem` (getAnswerUuid <$> getAllAnswers km)
  where
    getAnswerUuid answer = answer ^. ansUuid

------------------------------------------------------------------------------------------
ansFollowUpIds :: Answer -> [UUID]
ansFollowUpIds ans = ans ^.. ansFollowUps . traverse . qUuid

ansChangeFollowUpIdsOrder :: ([Question] -> Identity [UUID]) -> Answer -> Identity Answer
ansChangeFollowUpIdsOrder convert ans = Identity $ ans & ansFollowUps .~ orderedFollowUps
  where
    ids :: Identity [UUID]
    ids = convert (ans ^. ansFollowUps)
    orderedFollowUps :: [Question]
    orderedFollowUps = concatMap getFollowUpsByUuid (runIdentity ids)
    getFollowUpsByUuid :: UUID -> [Question]
    getFollowUpsByUuid uuid = filter (\x -> x ^. qUuid == uuid) (ans ^. ansFollowUps)

------------------------------------------------------------------------------------------
qExpertIds :: Question -> [UUID]
qExpertIds q = q ^.. qExperts . traverse . expUuid

qChangeExpertIdsOrder :: ([Expert] -> Identity [UUID]) -> Question -> Identity Question
qChangeExpertIdsOrder convert q = Identity $ q & qExperts .~ orderedExperts
  where
    ids :: Identity [UUID]
    ids = convert (q ^. qExperts)
    orderedExperts :: [Expert]
    orderedExperts = concatMap getExpertByUuid (runIdentity ids)
    getExpertByUuid :: UUID -> [Expert]
    getExpertByUuid uuid = filter (\x -> x ^. expUuid == uuid) (q ^. qExperts)

getAllExperts :: KnowledgeModel -> [Expert]
getAllExperts km = concat $ getExpert <$> getAllQuestions km
  where
    getExpert :: Question -> [Expert]
    getExpert question = question ^. qExperts

isThereAnyExpertWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyExpertWithGivenUuid km uuid = uuid `elem` (getExpertUuid <$> getAllExperts km)
  where
    getExpertUuid expert = expert ^. expUuid

------------------------------------------------------------------------------------------
qReferenceIds :: Question -> [UUID]
qReferenceIds q = q ^.. qReferences . traverse . refUuid

qChangeReferenceIdsOrder :: ([Reference] -> Identity [UUID]) -> Question -> Identity Question
qChangeReferenceIdsOrder convert q = Identity $ q & qReferences .~ orderedReferences
  where
    ids :: Identity [UUID]
    ids = convert (q ^. qReferences)
    orderedReferences :: [Reference]
    orderedReferences = concatMap getReferenceByUuid (runIdentity ids)
    getReferenceByUuid :: UUID -> [Reference]
    getReferenceByUuid uuid = filter (\x -> x ^. refUuid == uuid) (q ^. qReferences)

getAllReferences :: KnowledgeModel -> [Reference]
getAllReferences km = concat $ getReference <$> getAllQuestions km
  where
    getReference :: Question -> [Reference]
    getReference question = question ^. qReferences

isThereAnyReferenceWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyReferenceWithGivenUuid km uuid = uuid `elem` (getReferenceUuid <$> getAllReferences km)
  where
    getReferenceUuid reference = reference ^. refUuid
