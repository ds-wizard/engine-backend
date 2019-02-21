module Service.KnowledgeModel.KnowledgeModelFilter where

import Control.Lens ((&), (.~), (^.))
import Data.Maybe (catMaybes)
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses

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
  case filter (\qTagUuid -> qTagUuid `elem` selectedTagUuids) (getTagUuids q) of
    [] -> Nothing
    filteredQuestionTagUuids -> Just . applyToChildren $ q & qChangeTagUuids .~ filteredQuestionTagUuids
  where
    applyToChildren (OptionsQuestion' q) =
      OptionsQuestion' $ q & answers .~ (filterAnswers selectedTagUuids (q ^. answers))
    applyToChildren (ListQuestion' q) =
      ListQuestion' $ q & itemTemplateQuestions .~ (filterQuestions selectedTagUuids (q ^. itemTemplateQuestions))
    applyToChildren q = q

filterAnswers :: [U.UUID] -> [Answer] -> [Answer]
filterAnswers selectedTagUuids as = filterAnswer selectedTagUuids <$> as

filterAnswer :: [U.UUID] -> Answer -> Answer
filterAnswer selectedTagUuids a = a & followUps .~ (filterQuestions selectedTagUuids (a ^. followUps))
