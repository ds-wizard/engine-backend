module Service.KnowledgeModel.KnowledgeModelFilter where

import Control.Lens ((&), (.~), (^.))
import Data.Maybe (catMaybes)
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

filterKnowledgeModel :: [U.UUID] -> KnowledgeModel -> KnowledgeModel
filterKnowledgeModel [] km = km
filterKnowledgeModel selectedTagUuids km = (km & chapters .~ filteredChapters) & tags .~ filteredTags
  where
    filteredChapters = filterChapters selectedTagUuids (km ^. chapters)
    filteredTags = filter (\tag -> (tag ^. uuid) `elem` selectedTagUuids) (km ^. tags)

filterChapters :: [U.UUID] -> [Chapter] -> [Chapter]
filterChapters selectedTagUuids chs = catMaybes $ filterChapter selectedTagUuids <$> chs

filterChapter :: [U.UUID] -> Chapter -> Maybe Chapter
filterChapter selectedTagUuids ch =
  case filterQuestions selectedTagUuids (ch ^. questions) of
    [] -> Nothing
    qs -> Just $ ch & questions .~ qs

filterQuestions :: [U.UUID] -> [Question] -> [Question]
filterQuestions selectedTagUuids qs = catMaybes $ filterQuestion selectedTagUuids <$> qs

filterQuestion :: [U.UUID] -> Question -> Maybe Question
filterQuestion selectedTagUuids q =
  case filter (\qTagUuid -> qTagUuid `elem` selectedTagUuids) (q ^. tagUuids) of
    [] -> Nothing
    filteredQuestionTagUuids -> Just . applyToChildren $ q & tagUuids .~ filteredQuestionTagUuids
  where
    applyToChildren = applyToAnswers . applyToAnswerItemTemplate
    applyToAnswers q =
      case q ^. answers of
        Just as -> q & answers .~ (Just $ filterAnswers selectedTagUuids as)
        Nothing -> q
    applyToAnswerItemTemplate q =
      case q ^. answerItemTemplate of
        Just ait -> q & answerItemTemplate .~ (Just $ filterAnswerItemTemplate selectedTagUuids ait)
        Nothing -> q

filterAnswers :: [U.UUID] -> [Answer] -> [Answer]
filterAnswers selectedTagUuids as = filterAnswer selectedTagUuids <$> as

filterAnswer :: [U.UUID] -> Answer -> Answer
filterAnswer selectedTagUuids a = a & followUps .~ (filterQuestions selectedTagUuids (a ^. followUps))

filterAnswerItemTemplate :: [U.UUID] -> AnswerItemTemplate -> AnswerItemTemplate
filterAnswerItemTemplate selectedTagUuids ait = ait & questions .~ (filterQuestions selectedTagUuids (ait ^. questions))
