module Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels where

import Control.Lens

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.Uuid

km1 :: KnowledgeModel
km1 =
  KnowledgeModel
    { _knowledgeModelUuid = u' "ff672529-e837-4201-b7b1-7ada557d9725"
    , _knowledgeModelAnnotations = []
    , _knowledgeModelChapterUuids = [chapter1 ^. uuid, chapter2 ^. uuid, chapter3 ^. uuid]
    , _knowledgeModelTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _knowledgeModelIntegrationUuids = [ontologyPortal ^. uuid, bioPortal ^. uuid]
    , _knowledgeModelMetricUuids =
        [metricF ^. uuid, metricA ^. uuid, metricI ^. uuid, metricR ^. uuid, metricG ^. uuid, metricO ^. uuid]
    , _knowledgeModelPhaseUuids = [phase1 ^. uuid, phase2 ^. uuid, phase3 ^. uuid]
    , _knowledgeModelEntities =
        KnowledgeModelEntities
          { _knowledgeModelEntitiesChapters = toMap [chapter1, chapter2, chapter3]
          , _knowledgeModelEntitiesQuestions =
              toMap
                [ question1'
                , question2'
                , q2_aYes_fuQuestion1'
                , q2_aYes_fuq1_aYes_fuQuestion2'
                , question3'
                , question9'
                , question10'
                , question11'
                , question12'
                ]
          , _knowledgeModelEntitiesAnswers =
              toMap
                [ q2_answerNo
                , q2_answerYes
                , q2_aYes_fuq1_answerNo
                , q2_aYes_fuq1_answerYes
                , q2_aYes_fuq1_aYes_fuq2_answerNo
                , q2_aYes_fuq1_aYes_fuq2_answerYes
                , q3_answerNo
                , q3_answerYes
                ]
          , _knowledgeModelEntitiesChoices = toMap [q11_choice1, q11_choice2]
          , _knowledgeModelEntitiesExperts = toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola]
          , _knowledgeModelEntitiesReferences = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2']
          , _knowledgeModelEntitiesIntegrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , _knowledgeModelEntitiesTags = toMap [tagDataScience, tagBioInformatic]
          , _knowledgeModelEntitiesMetrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , _knowledgeModelEntitiesPhases = toMap [phase1, phase2, phase3]
          }
    }

km1Edited :: KnowledgeModel
km1Edited =
  KnowledgeModel
    { _knowledgeModelUuid = km1 ^. uuid
    , _knowledgeModelAnnotations = [MapEntry "newAnnotation" "someValue"]
    , _knowledgeModelChapterUuids = [chapter3 ^. uuid, chapter2 ^. uuid, chapter1 ^. uuid]
    , _knowledgeModelTagUuids = [tagBioInformatic ^. uuid, tagDataScience ^. uuid]
    , _knowledgeModelIntegrationUuids = [widgetPortal ^. uuid, bioPortal ^. uuid, ontologyPortal ^. uuid]
    , _knowledgeModelMetricUuids =
        [metricF ^. uuid, metricA ^. uuid, metricI ^. uuid, metricR ^. uuid, metricG ^. uuid, metricO ^. uuid]
    , _knowledgeModelPhaseUuids = [phase1 ^. uuid, phase2 ^. uuid, phase3 ^. uuid]
    , _knowledgeModelEntities = km1 ^. entities
    }

km1WithoutChaptersAndTagsAndIntegrations :: KnowledgeModel
km1WithoutChaptersAndTagsAndIntegrations =
  KnowledgeModel
    { _knowledgeModelUuid = km1 ^. uuid
    , _knowledgeModelAnnotations = []
    , _knowledgeModelChapterUuids = []
    , _knowledgeModelTagUuids = []
    , _knowledgeModelIntegrationUuids = []
    , _knowledgeModelMetricUuids = []
    , _knowledgeModelPhaseUuids = []
    , _knowledgeModelEntities =
        KnowledgeModelEntities
          { _knowledgeModelEntitiesChapters = toMap []
          , _knowledgeModelEntitiesQuestions = toMap []
          , _knowledgeModelEntitiesAnswers = toMap []
          , _knowledgeModelEntitiesChoices = toMap []
          , _knowledgeModelEntitiesExperts = toMap []
          , _knowledgeModelEntitiesReferences = toMap []
          , _knowledgeModelEntitiesIntegrations = toMap []
          , _knowledgeModelEntitiesTags = toMap []
          , _knowledgeModelEntitiesMetrics = toMap []
          , _knowledgeModelEntitiesPhases = toMap []
          }
    }

km1WithQ4Plain :: KnowledgeModel
km1WithQ4Plain =
  KnowledgeModel
    { _knowledgeModelUuid = km1 ^. uuid
    , _knowledgeModelAnnotations = []
    , _knowledgeModelChapterUuids = [chapter1 ^. uuid, chapter2WithQ4Plain ^. uuid, chapter3 ^. uuid]
    , _knowledgeModelTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _knowledgeModelIntegrationUuids = [ontologyPortal ^. uuid, bioPortal ^. uuid, widgetPortal ^. uuid]
    , _knowledgeModelMetricUuids =
        [metricF ^. uuid, metricA ^. uuid, metricI ^. uuid, metricR ^. uuid, metricG ^. uuid, metricO ^. uuid]
    , _knowledgeModelPhaseUuids = [phase1 ^. uuid, phase2 ^. uuid, phase3 ^. uuid]
    , _knowledgeModelEntities =
        KnowledgeModelEntities
          { _knowledgeModelEntitiesChapters = toMap [chapter1, chapter2WithQ4Plain, chapter3]
          , _knowledgeModelEntitiesQuestions =
              toMap
                [ question1'
                , question2'
                , q2_aYes_fuQuestion1'
                , q2_aYes_fuq1_aYes_fuQuestion2'
                , question3'
                , question4Plain'
                , question9'
                , question10'
                , question11'
                , question12'
                ]
          , _knowledgeModelEntitiesAnswers =
              toMap
                [ q2_answerNo
                , q2_answerYes
                , q2_aYes_fuq1_answerNo
                , q2_aYes_fuq1_answerYes
                , q2_aYes_fuq1_aYes_fuq2_answerNo
                , q2_aYes_fuq1_aYes_fuq2_answerYes
                , q3_answerNo
                , q3_answerYes
                ]
          , _knowledgeModelEntitiesChoices = toMap [q11_choice1, q11_choice2]
          , _knowledgeModelEntitiesExperts = toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola]
          , _knowledgeModelEntitiesReferences = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2']
          , _knowledgeModelEntitiesIntegrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , _knowledgeModelEntitiesTags = toMap [tagDataScience, tagBioInformatic]
          , _knowledgeModelEntitiesMetrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , _knowledgeModelEntitiesPhases = toMap [phase1, phase2, phase3]
          }
    }

km1WithQ4 :: KnowledgeModel
km1WithQ4 =
  KnowledgeModel
    { _knowledgeModelUuid = km1 ^. uuid
    , _knowledgeModelAnnotations = []
    , _knowledgeModelChapterUuids = [chapter1 ^. uuid, chapter2WithQ4 ^. uuid, chapter3 ^. uuid]
    , _knowledgeModelTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _knowledgeModelIntegrationUuids = [ontologyPortal ^. uuid, bioPortal ^. uuid, widgetPortal ^. uuid]
    , _knowledgeModelMetricUuids =
        [metricF ^. uuid, metricA ^. uuid, metricI ^. uuid, metricR ^. uuid, metricG ^. uuid, metricO ^. uuid]
    , _knowledgeModelPhaseUuids = [phase1 ^. uuid, phase2 ^. uuid, phase3 ^. uuid]
    , _knowledgeModelEntities =
        KnowledgeModelEntities
          { _knowledgeModelEntitiesChapters = toMap [chapter1, chapter2WithQ4, chapter3]
          , _knowledgeModelEntitiesQuestions =
              toMap
                [ question1'
                , question2'
                , q2_aYes_fuQuestion1'
                , q2_aYes_fuq1_aYes_fuQuestion2'
                , question3'
                , question4'
                , q4_it1_question5'
                , q4_it1_q5_it2_question7'
                , q4_it1_q5_it2_question8'
                , q4_it1_question6'
                , q4_it1_q6_aYes_followUpQuestion4'
                , q4_it1_q6_aYes_fuq4_it_question1'
                , q4_it1_q6_aYes_fuq4_it_question2'
                , q4_it1_q6_aYes_followUpQuestion5'
                , question9'
                , question10'
                , question11'
                , question12'
                ]
          , _knowledgeModelEntitiesAnswers =
              toMap
                [ q2_answerNo
                , q2_answerYes
                , q3_answerNo
                , q3_answerYes
                , q2_aYes_fuq1_answerNo
                , q2_aYes_fuq1_answerYes
                , q2_aYes_fuq1_aYes_fuq2_answerNo
                , q2_aYes_fuq1_aYes_fuq2_answerYes
                , q4_it1_q6_answerNo
                , q4_it1_q6_answerYes
                ]
          , _knowledgeModelEntitiesChoices = toMap [q11_choice1, q11_choice2]
          , _knowledgeModelEntitiesExperts =
              toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola, km1_ch2_q6_eAlbert, km1_ch2_q6_eNikola]
          , _knowledgeModelEntitiesReferences = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2', km1_ch2_q6_r1', km1_ch2_q6_r2']
          , _knowledgeModelEntitiesIntegrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , _knowledgeModelEntitiesTags = toMap [tagDataScience, tagBioInformatic]
          , _knowledgeModelEntitiesMetrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , _knowledgeModelEntitiesPhases = toMap [phase1, phase2, phase3]
          }
    }

km1Global :: KnowledgeModel
km1Global =
  KnowledgeModel
    { _knowledgeModelUuid = km1 ^. uuid
    , _knowledgeModelAnnotations = []
    , _knowledgeModelChapterUuids = []
    , _knowledgeModelTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _knowledgeModelIntegrationUuids = [ontologyPortal ^. uuid, bioPortal ^. uuid, widgetPortal ^. uuid]
    , _knowledgeModelMetricUuids =
        [metricF ^. uuid, metricA ^. uuid, metricI ^. uuid, metricR ^. uuid, metricG ^. uuid, metricO ^. uuid]
    , _knowledgeModelPhaseUuids = [phase1 ^. uuid, phase2 ^. uuid, phase3 ^. uuid]
    , _knowledgeModelEntities =
        KnowledgeModelEntities
          { _knowledgeModelEntitiesChapters = toMap []
          , _knowledgeModelEntitiesQuestions = toMap []
          , _knowledgeModelEntitiesAnswers = toMap []
          , _knowledgeModelEntitiesChoices = toMap []
          , _knowledgeModelEntitiesExperts = toMap []
          , _knowledgeModelEntitiesReferences = toMap []
          , _knowledgeModelEntitiesIntegrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , _knowledgeModelEntitiesTags = toMap [tagDataScience, tagBioInformatic]
          , _knowledgeModelEntitiesMetrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , _knowledgeModelEntitiesPhases = toMap [phase1, phase2, phase3]
          }
    }

km1Netherlands :: KnowledgeModel
km1Netherlands =
  KnowledgeModel
    { _knowledgeModelUuid = km1 ^. uuid
    , _knowledgeModelAnnotations = []
    , _knowledgeModelChapterUuids = [chapter1WithoutQuestions ^. uuid]
    , _knowledgeModelTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _knowledgeModelIntegrationUuids = [ontologyPortal ^. uuid, bioPortal ^. uuid, widgetPortal ^. uuid]
    , _knowledgeModelMetricUuids =
        [metricF ^. uuid, metricA ^. uuid, metricI ^. uuid, metricR ^. uuid, metricG ^. uuid, metricO ^. uuid]
    , _knowledgeModelPhaseUuids = [phase1 ^. uuid, phase2 ^. uuid, phase3 ^. uuid]
    , _knowledgeModelEntities =
        KnowledgeModelEntities
          { _knowledgeModelEntitiesChapters = toMap [chapter1WithoutQuestions]
          , _knowledgeModelEntitiesQuestions = toMap []
          , _knowledgeModelEntitiesAnswers = toMap []
          , _knowledgeModelEntitiesChoices = toMap []
          , _knowledgeModelEntitiesExperts = toMap []
          , _knowledgeModelEntitiesReferences = toMap []
          , _knowledgeModelEntitiesIntegrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , _knowledgeModelEntitiesTags = toMap [tagDataScience, tagBioInformatic]
          , _knowledgeModelEntitiesMetrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , _knowledgeModelEntitiesPhases = toMap [phase1, phase2, phase3]
          }
    }

km1NetherlandsV2 :: KnowledgeModel
km1NetherlandsV2 =
  KnowledgeModel
    { _knowledgeModelUuid = km1 ^. uuid
    , _knowledgeModelAnnotations = []
    , _knowledgeModelChapterUuids = [chapter1WithoutQuestions ^. uuid, chapter4WithoutQuestions ^. uuid]
    , _knowledgeModelTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _knowledgeModelIntegrationUuids = [ontologyPortal ^. uuid, bioPortal ^. uuid, widgetPortal ^. uuid]
    , _knowledgeModelMetricUuids =
        [metricF ^. uuid, metricA ^. uuid, metricI ^. uuid, metricR ^. uuid, metricG ^. uuid, metricO ^. uuid]
    , _knowledgeModelPhaseUuids = [phase1 ^. uuid, phase2 ^. uuid, phase3 ^. uuid]
    , _knowledgeModelEntities =
        KnowledgeModelEntities
          { _knowledgeModelEntitiesChapters = toMap [chapter1WithoutQuestions, chapter4WithoutQuestions]
          , _knowledgeModelEntitiesQuestions = toMap []
          , _knowledgeModelEntitiesAnswers = toMap []
          , _knowledgeModelEntitiesChoices = toMap []
          , _knowledgeModelEntitiesExperts = toMap []
          , _knowledgeModelEntitiesReferences = toMap []
          , _knowledgeModelEntitiesIntegrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , _knowledgeModelEntitiesTags = toMap [tagDataScience, tagBioInformatic]
          , _knowledgeModelEntitiesMetrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , _knowledgeModelEntitiesPhases = toMap [phase1, phase2, phase3]
          }
    }
