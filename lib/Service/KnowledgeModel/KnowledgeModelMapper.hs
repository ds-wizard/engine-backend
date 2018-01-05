module Service.KnowledgeModel.KnowledgeModelMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Types
import Model.KnowledgeModel.KnowledgeModel

toKnowledgeModelDTO :: KnowledgeModel -> KnowledgeModelDTO
toKnowledgeModelDTO km =
  KnowledgeModelDTO
  {_kmdtoUuid = km ^. kmUuid, _kmdtoName = km ^. kmName, _kmdtoChapters = toChapterDTO <$> (km ^. kmChapters)}

toChapterDTO :: Chapter -> ChapterDTO
toChapterDTO chapter =
  ChapterDTO
  { _chdtoUuid = chapter ^. chUuid
  , _chdtoTitle = chapter ^. chTitle
  , _chdtoText = chapter ^. chText
  , _chdtoQuestions = toQuestionDTO <$> (chapter ^. chQuestions)
  }

toQuestionDTO :: Question -> QuestionDTO
toQuestionDTO question =
  QuestionDTO
  { _qdtoUuid = question ^. qUuid
  , _qdtoShortUuid = question ^. qShortUuid
  , _qdtoType = question ^. qType
  , _qdtoTitle = question ^. qTitle
  , _qdtoText = question ^. qText
  , _qdtoAnswers = toAnswerDTO <$> (question ^. qAnswers)
  , _qdtoExperts = toExpertDTO <$> (question ^. qExperts)
  , _qdtoReferences = toReferenceDTO <$> (question ^. qReferences)
  }

toAnswerDTO :: Answer -> AnswerDTO
toAnswerDTO answer =
  AnswerDTO
  { _ansdtoUuid = answer ^. ansUuid
  , _ansdtoLabel = answer ^. ansLabel
  , _ansdtoAdvice = answer ^. ansAdvice
  , _ansdtoFollowUps = toQuestionDTO <$> (answer ^. ansFollowUps)
  }

toExpertDTO :: Expert -> ExpertDTO
toExpertDTO expert =
  ExpertDTO {_expdtoUuid = expert ^. expUuid, _expdtoName = expert ^. expName, _expdtoEmail = expert ^. expEmail}

toReferenceDTO :: Reference -> ReferenceDTO
toReferenceDTO reference = ReferenceDTO {_refdtoUuid = reference ^. refUuid, _refdtoChapter = reference ^. refChapter}

fromKnowledgeModelDTO :: KnowledgeModelDTO -> KnowledgeModel
fromKnowledgeModelDTO km =
  KnowledgeModel
  {_kmUuid = km ^. kmdtoUuid, _kmName = km ^. kmdtoName, _kmChapters = fromChapterDTO <$> (km ^. kmdtoChapters)}

fromChapterDTO :: ChapterDTO -> Chapter
fromChapterDTO chapter =
  Chapter
  { _chUuid = chapter ^. chdtoUuid
  , _chTitle = chapter ^. chdtoTitle
  , _chText = chapter ^. chdtoText
  , _chQuestions = fromQuestionDTO <$> (chapter ^. chdtoQuestions)
  }

fromQuestionDTO :: QuestionDTO -> Question
fromQuestionDTO question =
  Question
  { _qUuid = question ^. qdtoUuid
  , _qShortUuid = question ^. qdtoShortUuid
  , _qType = question ^. qdtoType
  , _qTitle = question ^. qdtoTitle
  , _qText = question ^. qdtoText
  , _qAnswers = fromAnswerDTO <$> (question ^. qdtoAnswers)
  , _qExperts = fromExpertDTO <$> (question ^. qdtoExperts)
  , _qReferences = fromReferenceDTO <$> (question ^. qdtoReferences)
  }

fromAnswerDTO :: AnswerDTO -> Answer
fromAnswerDTO answer =
  Answer
  { _ansUuid = answer ^. ansdtoUuid
  , _ansLabel = answer ^. ansdtoLabel
  , _ansAdvice = answer ^. ansdtoAdvice
  , _ansFollowUps = fromQuestionDTO <$> (answer ^. ansdtoFollowUps)
  }

fromExpertDTO :: ExpertDTO -> Expert
fromExpertDTO expert =
  Expert {_expUuid = expert ^. expdtoUuid, _expName = expert ^. expdtoName, _expEmail = expert ^. expdtoEmail}

fromReferenceDTO :: ReferenceDTO -> Reference
fromReferenceDTO reference = Reference {_refUuid = reference ^. refdtoUuid, _refChapter = reference ^. refdtoChapter}
