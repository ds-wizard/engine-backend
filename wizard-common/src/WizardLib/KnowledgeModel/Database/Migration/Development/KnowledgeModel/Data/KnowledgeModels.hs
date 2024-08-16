module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels where

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Choices
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Metrics
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Resources
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

km1 :: KnowledgeModel
km1 =
  KnowledgeModel
    { uuid = u' "ff672529-e837-4201-b7b1-7ada557d9725"
    , annotations = []
    , chapterUuids = [chapter1.uuid, chapter2.uuid, chapter3.uuid]
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , integrationUuids = [ontologyPortal.uuid, bioPortal.uuid]
    , metricUuids =
        [metricF.uuid, metricA.uuid, metricI.uuid, metricR.uuid, metricG.uuid, metricO.uuid]
    , phaseUuids = [phase1.uuid, phase2.uuid, phase3.uuid]
    , resourceCollectionUuids = [rc1.uuid, rc2.uuid]
    , entities =
        KnowledgeModelEntities
          { chapters = toMap [chapter1, chapter2, chapter3]
          , questions =
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
          , answers =
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
          , choices = toMap [q11_choice1, q11_choice2]
          , experts = toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola]
          , references = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2']
          , integrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , tags = toMap [tagDataScience, tagBioInformatic]
          , metrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , phases = toMap [phase1, phase2, phase3]
          , resourceCollections = toMap [rc1, rc2]
          , resourcePages = toMap [rc1_rp1, rc1_rp2, rc2_rp1]
          }
    }

km1Edited :: KnowledgeModel
km1Edited =
  KnowledgeModel
    { uuid = km1.uuid
    , annotations = [MapEntry "newAnnotation" "someValue"]
    , chapterUuids = [chapter3.uuid, chapter2.uuid, chapter1.uuid]
    , tagUuids = [tagBioInformatic.uuid, tagDataScience.uuid]
    , integrationUuids = [widgetPortal.uuid, bioPortal.uuid, ontologyPortal.uuid]
    , metricUuids =
        [metricF.uuid, metricA.uuid, metricI.uuid, metricR.uuid, metricG.uuid, metricO.uuid]
    , phaseUuids = [phase1.uuid, phase2.uuid, phase3.uuid]
    , resourceCollectionUuids = [rc2.uuid, rc1.uuid]
    , entities = km1.entities
    }

km1WithoutChaptersAndTagsAndIntegrations :: KnowledgeModel
km1WithoutChaptersAndTagsAndIntegrations =
  KnowledgeModel
    { uuid = km1.uuid
    , annotations = []
    , chapterUuids = []
    , tagUuids = []
    , integrationUuids = []
    , metricUuids = []
    , phaseUuids = []
    , resourceCollectionUuids = []
    , entities =
        KnowledgeModelEntities
          { chapters = toMap []
          , questions = toMap []
          , answers = toMap []
          , choices = toMap []
          , experts = toMap []
          , references = toMap []
          , integrations = toMap []
          , tags = toMap []
          , metrics = toMap []
          , phases = toMap []
          , resourceCollections = toMap []
          , resourcePages = toMap []
          }
    }

km1WithQ4Plain :: KnowledgeModel
km1WithQ4Plain =
  KnowledgeModel
    { uuid = km1.uuid
    , annotations = []
    , chapterUuids = [chapter1.uuid, chapter2WithQ4Plain.uuid, chapter3.uuid]
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , integrationUuids = [ontologyPortal.uuid, bioPortal.uuid, widgetPortal.uuid]
    , metricUuids =
        [metricF.uuid, metricA.uuid, metricI.uuid, metricR.uuid, metricG.uuid, metricO.uuid]
    , phaseUuids = [phase1.uuid, phase2.uuid, phase3.uuid]
    , resourceCollectionUuids = [rc1.uuid, rc2.uuid]
    , entities =
        KnowledgeModelEntities
          { chapters = toMap [chapter1, chapter2WithQ4Plain, chapter3]
          , questions =
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
          , answers =
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
          , choices = toMap [q11_choice1, q11_choice2]
          , experts = toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola]
          , references = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2']
          , integrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , tags = toMap [tagDataScience, tagBioInformatic]
          , metrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , phases = toMap [phase1, phase2, phase3]
          , resourceCollections = toMap [rc1, rc2]
          , resourcePages = toMap [rc1_rp1, rc1_rp2, rc2_rp1]
          }
    }

km1WithQ4 :: KnowledgeModel
km1WithQ4 =
  KnowledgeModel
    { uuid = km1.uuid
    , annotations = []
    , chapterUuids = [chapter1.uuid, chapter2WithQ4.uuid, chapter3.uuid]
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , integrationUuids = [ontologyPortal.uuid, bioPortal.uuid, widgetPortal.uuid]
    , metricUuids =
        [metricF.uuid, metricA.uuid, metricI.uuid, metricR.uuid, metricG.uuid, metricO.uuid]
    , phaseUuids = [phase1.uuid, phase2.uuid, phase3.uuid]
    , resourceCollectionUuids = [rc1.uuid, rc2.uuid]
    , entities =
        KnowledgeModelEntities
          { chapters = toMap [chapter1, chapter2WithQ4, chapter3]
          , questions =
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
          , answers =
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
          , choices = toMap [q11_choice1, q11_choice2]
          , experts =
              toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola, km1_ch2_q6_eAlbert, km1_ch2_q6_eNikola]
          , references = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2', km1_ch2_q6_r1', km1_ch2_q6_r2']
          , integrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , tags = toMap [tagDataScience, tagBioInformatic]
          , metrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , phases = toMap [phase1, phase2, phase3]
          , resourceCollections = toMap [rc1, rc2]
          , resourcePages = toMap [rc1_rp1, rc1_rp2, rc2_rp1]
          }
    }

km1Global :: KnowledgeModel
km1Global =
  KnowledgeModel
    { uuid = km1.uuid
    , annotations = []
    , chapterUuids = []
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , integrationUuids = [ontologyPortal.uuid, bioPortal.uuid, widgetPortal.uuid]
    , metricUuids =
        [metricF.uuid, metricA.uuid, metricI.uuid, metricR.uuid, metricG.uuid, metricO.uuid]
    , phaseUuids = [phase1.uuid, phase2.uuid, phase3.uuid]
    , resourceCollectionUuids = [rc1.uuid, rc2.uuid]
    , entities =
        KnowledgeModelEntities
          { chapters = toMap []
          , questions = toMap []
          , answers = toMap []
          , choices = toMap []
          , experts = toMap []
          , references = toMap []
          , integrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , tags = toMap [tagDataScience, tagBioInformatic]
          , metrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , phases = toMap [phase1, phase2, phase3]
          , resourceCollections = toMap [rc1, rc2]
          , resourcePages = toMap [rc1_rp1, rc1_rp2, rc2_rp1]
          }
    }

km1Netherlands :: KnowledgeModel
km1Netherlands =
  KnowledgeModel
    { uuid = km1.uuid
    , annotations = []
    , chapterUuids = [chapter1WithoutQuestions.uuid]
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , integrationUuids = [ontologyPortal.uuid, bioPortal.uuid, widgetPortal.uuid]
    , metricUuids =
        [metricF.uuid, metricA.uuid, metricI.uuid, metricR.uuid, metricG.uuid, metricO.uuid]
    , phaseUuids = [phase1.uuid, phase2.uuid, phase3.uuid]
    , resourceCollectionUuids = [rc1.uuid, rc2.uuid]
    , entities =
        KnowledgeModelEntities
          { chapters = toMap [chapter1WithoutQuestions]
          , questions = toMap []
          , answers = toMap []
          , choices = toMap []
          , experts = toMap []
          , references = toMap []
          , integrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , tags = toMap [tagDataScience, tagBioInformatic]
          , metrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , phases = toMap [phase1, phase2, phase3]
          , resourceCollections = toMap [rc1, rc2]
          , resourcePages = toMap [rc1_rp1, rc1_rp2, rc2_rp1]
          }
    }

km1NetherlandsV2 :: KnowledgeModel
km1NetherlandsV2 =
  KnowledgeModel
    { uuid = km1.uuid
    , annotations = []
    , chapterUuids = [chapter1WithoutQuestions.uuid, chapter4WithoutQuestions.uuid]
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , integrationUuids = [ontologyPortal.uuid, bioPortal.uuid, widgetPortal.uuid]
    , metricUuids =
        [metricF.uuid, metricA.uuid, metricI.uuid, metricR.uuid, metricG.uuid, metricO.uuid]
    , phaseUuids = [phase1.uuid, phase2.uuid, phase3.uuid]
    , resourceCollectionUuids = [rc1.uuid, rc2.uuid]
    , entities =
        KnowledgeModelEntities
          { chapters = toMap [chapter1WithoutQuestions, chapter4WithoutQuestions]
          , questions = toMap []
          , answers = toMap []
          , choices = toMap []
          , experts = toMap []
          , references = toMap []
          , integrations = toMap [ontologyPortal', bioPortal', widgetPortal']
          , tags = toMap [tagDataScience, tagBioInformatic]
          , metrics = toMap [metricF, metricA, metricI, metricR, metricG, metricO]
          , phases = toMap [phase1, phase2, phase3]
          , resourceCollections = toMap [rc1, rc2]
          , resourcePages = toMap [rc1_rp1, rc1_rp2, rc2_rp1]
          }
    }
