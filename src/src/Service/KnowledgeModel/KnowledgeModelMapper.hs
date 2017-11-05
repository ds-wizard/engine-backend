module Service.KnowledgeModel.KnowledgeModelMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.KnowledgeModel.KnowledgeModelDTO
import Common.Types
import Model.KnowledgeModel.KnowledgeModel

toKnowledgeModelDTO :: KnowledgeModel -> KnowledgeModelDTO
toKnowledgeModelDTO km =
  KnowledgeModelDTO
  { _kmdtoUuid = km ^. kmUuid
  , _kmdtoName = km ^. kmName
  , _kmdtoChapters = toChapterDTO <$> (km ^. kmChapters)
  }

toChapterDTO :: Chapter -> ChapterDTO
toChapterDTO chapter =
  ChapterDTO
  { _chdtoUuid = chapter ^. chUuid
  , _chdtoNamespace = chapter ^. chNamespace
  , _chdtoFormatVersion = chapter ^. chFormatVersion
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
  , _ansdtoFollowing = toQuestionDTO <$> (answer ^. ansFollowing)
  }

toExpertDTO :: Expert -> ExpertDTO
toExpertDTO expert =
  ExpertDTO
  { _expdtoUuid = expert ^. expUuid
  , _expdtoName = expert ^. expName
  , _expdtoEmail = expert ^. expEmail
  }

toReferenceDTO :: Reference -> ReferenceDTO
toReferenceDTO reference =
  ReferenceDTO
  {_refdtoUuid = reference ^. refUuid, _refdtoChapter = reference ^. refChapter}
