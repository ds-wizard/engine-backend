module Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
  ( chapter1
  , chapter1Edited
  , chapter1WithChangedQuestion2
  , chapter1WithoutQuestions
  , chapter1WithAddedQuestion3
  , chapter2
  , chapter2WithoutQuestions
  , chapter2WithQ4Plain
  , chapter2WithQ4
  , chapter3
  , chapter4WithoutQuestions
  ) where

import Control.Lens
import qualified Data.Map.Strict as M

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

chapter1 :: Chapter
chapter1 =
  Chapter
    { _chapterUuid = u' "e3c4b314-919b-418d-bb85-a07c97433427"
    , _chapterTitle = "Design of experiment"
    , _chapterText =
        Just
          "Before you decide to embark on any new study, it is nowadays good practice to consider all options to keep the data generation part of your study as limited as possible. It is not because we can generate massive amounts of data that we always need to do so. Creating data with public money is bringing with it the responsibility to treat those data well and (if potentially useful) make them available for re-use by others."
    , _chapterAnnotations = M.empty
    , _chapterQuestionUuids = [question1 ^. uuid, question2 ^. uuid]
    }

chapter1Edited :: Chapter
chapter1Edited =
  chapter1
    { _chapterTitle = "EDITED: " ++ (chapter1 ^. title)
    , _chapterText = ("EDITED: " ++) <$> (chapter1 ^. text)
    , _chapterAnnotations = M.fromList [("newAnnotation", "someValue")]
    , _chapterQuestionUuids = [question2 ^. uuid, question1 ^. uuid]
    }

chapter1WithChangedQuestion2 :: Chapter
chapter1WithChangedQuestion2 = chapter1 {_chapterQuestionUuids = [question1 ^. uuid, question2Edited ^. uuid]}

chapter1WithoutQuestions :: Chapter
chapter1WithoutQuestions = chapter1 {_chapterQuestionUuids = []}

chapter1WithAddedQuestion3 :: Chapter
chapter1WithAddedQuestion3 = chapter1 {_chapterQuestionUuids = chapter1 ^. questionUuids ++ [question3Plain ^. uuid]}

chapter2 :: Chapter
chapter2 =
  Chapter
    { _chapterUuid = u' "a2a25254-58e3-4fc7-9850-160803d792fe"
    , _chapterTitle = "Data design and planning"
    , _chapterText =
        Just
          "In the data design and planning phase, we will make sure that we know what data comes when, that we have enough storage space and compute power to deal with it, and that all the responsibilities have been taken care of."
    , _chapterAnnotations = M.empty
    , _chapterQuestionUuids = [question3 ^. uuid]
    }

chapter2WithoutQuestions :: Chapter
chapter2WithoutQuestions = chapter2 {_chapterQuestionUuids = []}

chapter2WithQ4Plain :: Chapter
chapter2WithQ4Plain = chapter2 {_chapterQuestionUuids = [question3 ^. uuid, question4Plain ^. uuid]}

chapter2WithQ4 :: Chapter
chapter2WithQ4 = chapter2 {_chapterQuestionUuids = [question3 ^. uuid, question4 ^. uuid]}

chapter3 :: Chapter
chapter3 =
  Chapter
    { _chapterUuid = u' "ca46cd40-3999-4f0d-a8ba-15d57682dfeb"
    , _chapterTitle = "Data Capture/Measurement"
    , _chapterText = Just ""
    , _chapterAnnotations = M.empty
    , _chapterQuestionUuids = [question9 ^. uuid, question10 ^. uuid, question11 ^. uuid, question12 ^. uuid]
    }

chapter4WithoutQuestions :: Chapter
chapter4WithoutQuestions =
  Chapter
    { _chapterUuid = u' "c0958799-16bb-41c2-a0ef-182d6709f0bb"
    , _chapterTitle = "Data processing and curation"
    , _chapterText = Just ""
    , _chapterAnnotations = M.empty
    , _chapterQuestionUuids = []
    }
