module Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters (
  chapter1,
  chapter1Edited,
  chapter1WithChangedQuestion2,
  chapter1WithoutQuestions,
  chapter1WithAddedQuestion3,
  chapter2,
  chapter2WithoutQuestions,
  chapter2WithQ4Plain,
  chapter2WithQ4,
  chapter3,
  chapter4WithoutQuestions,
) where

import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

chapter1 :: Chapter
chapter1 =
  Chapter
    { uuid = u' "00000000-0000-0000-0000-0000000000c1"
    , title = "Design of experiment"
    , text =
        Just
          "Before you decide to embark on any new study, it is nowadays good practice to consider all options to keep the data generation part of your study as limited as possible. It is not because we can generate massive amounts of data that we always need to do so. Creating data with public money is bringing with it the responsibility to treat those data well and (if potentially useful) make them available for re-use by others."
    , annotations = []
    , questionUuids = [question1.uuid, question2.uuid]
    }

chapter1Edited :: Chapter
chapter1Edited =
  chapter1
    { title = "EDITED: " ++ chapter1.title
    , text = ("EDITED: " ++) <$> chapter1.text
    , annotations = [MapEntry "newAnnotation" "someValue"]
    , questionUuids = [question2.uuid, question1.uuid]
    }

chapter1WithChangedQuestion2 :: Chapter
chapter1WithChangedQuestion2 = chapter1 {questionUuids = [question1.uuid, question2Edited.uuid]}

chapter1WithoutQuestions :: Chapter
chapter1WithoutQuestions = chapter1 {questionUuids = []}

chapter1WithAddedQuestion3 :: Chapter
chapter1WithAddedQuestion3 = chapter1 {questionUuids = chapter1.questionUuids ++ [question3Plain.uuid]}

chapter2 :: Chapter
chapter2 =
  Chapter
    { uuid = u' "00000000-0000-0000-0000-0000000000c2"
    , title = "Data design and planning"
    , text =
        Just
          "In the data design and planning phase, we will make sure that we know what data comes when, that we have enough storage space and compute power to deal with it, and that all the responsibilities have been taken care of."
    , annotations = []
    , questionUuids = [question3.uuid]
    }

chapter2WithoutQuestions :: Chapter
chapter2WithoutQuestions = chapter2 {questionUuids = []}

chapter2WithQ4Plain :: Chapter
chapter2WithQ4Plain = chapter2 {questionUuids = [question3.uuid, question4Plain.uuid]}

chapter2WithQ4 :: Chapter
chapter2WithQ4 = chapter2 {questionUuids = [question3.uuid, question4.uuid]}

chapter3 :: Chapter
chapter3 =
  Chapter
    { uuid = u' "00000000-0000-0000-0000-0000000000c3"
    , title = "Data Capture/Measurement"
    , text = Just ""
    , annotations = []
    , questionUuids = [question9.uuid, question10.uuid, question11.uuid, question12.uuid]
    }

chapter4WithoutQuestions :: Chapter
chapter4WithoutQuestions =
  Chapter
    { uuid = u' "00000000-0000-0000-0000-0000000000c4"
    , title = "Data processing and curation"
    , text = Just ""
    , annotations = []
    , questionUuids = []
    }
