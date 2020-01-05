module Wizard.Specs.Service.Migration.Questionnaire.MoveSanitizatorSpec where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.Questionnaire.QuestionnaireUtil
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.Migration.Questionnaire.Migrator.MoveSanitizator

sanitizatorSpec =
  describe "MoveSanatizor" $ do
    let s = U.toString
    let s' = fmap s
    let i' = createReplyKey
    describe "generateEvents" $
      it "Succeed" $
        -- GIVEN:
       do
        let (Right oldKm) = compile Nothing (germanyPackage ^. events)
        let moveEvents =
              [ MoveQuestionEvent' m_km1_ch1_q1__to_ch2
              , MoveQuestionEvent' m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo
              , MoveQuestionEvent' m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4
              , MoveAnswerEvent' m_km1_ch1_q2_aYes__to_ch2_q3
              ]
        let (Right newKm) = compile Nothing ((germanyPackage ^. events) ++ moveEvents)
        let expected =
              [ MoveQuestionEvent' (m_km1_ch1_q1__to_ch2 & uuid .~ U.nil)
              , MoveQuestionEvent' (m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo & uuid .~ U.nil)
              , MoveQuestionEvent' (m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 & uuid .~ U.nil)
              , MoveAnswerEvent' (m_km1_ch1_q2_aYes__to_ch2_q3 & uuid .~ U.nil)
              ]
        -- WHEN:
        let result = generateEvents oldKm newKm
        -- THEN:
        result `shouldBe` expected
    describe "computeParentPath" $ do
      createComputeParentPathTest (chapter1 ^. uuid) [chapter1 ^. uuid]
      createComputeParentPathTest (q4_it1_q6_aYes_followUpQuestion4 ^. uuid) q4_it1_q6_aYes_followUpQuestion4_path
    describe "computeSharedNode" $ do
      it "Nothing in common" $
        -- GIVEN:
       do
        let aPath = [chapter1 ^. uuid]
        let bPath = [chapter2 ^. uuid, question3 ^. uuid, q3_answerNo ^. uuid]
        let expected = Nothing
        -- WHEN:
        let result = computeSharedNode aPath bPath
        -- THEN:
        result `shouldBe` expected
      it "One node in common" $
        -- GIVEN:
       do
        let aPath = q4_it1_q6_aYes_followUpQuestion4_path
        let bPath = [chapter2 ^. uuid, question4 ^. uuid]
        let expected = Just (question4 ^. uuid)
        -- WHEN:
        let result = computeSharedNode aPath bPath
        -- THEN:
        result `shouldBe` expected
    describe "takeSharedPrefix" $ do
      it "Nothing in common" $
        -- GIVEN:
       do
        let path = q4_it1_q6_aYes_followUpQuestion4_path
        let sharedNode = Nothing
        let expected = path
        -- WHEN:
        let result = takeSharedPrefix sharedNode path
        -- THEN:
        result `shouldBe` expected
      it "One node in common" $
        -- GIVEN:
       do
        let path = q4_it1_q6_aYes_followUpQuestion4_path
        let sharedNode = Just (question4 ^. uuid)
        let expected = [chapter2 ^. uuid]
        -- WHEN:
        let result = takeSharedPrefix sharedNode path
        -- THEN:
        result `shouldBe` expected
    describe "takeDiffSuffix" $ do
      it "Nothing in common" $
        -- GIVEN:
       do
        let path = q4_it1_q6_aYes_followUpQuestion4_path
        let sharedNode = Nothing
        let expected = path
        -- WHEN:
        let result = takeDiffSuffix sharedNode path
        -- THEN:
        result `shouldBe` expected
      it "One node in common" $
        -- GIVEN:
       do
        let path = q4_it1_q6_aYes_followUpQuestion4_path
        let sharedNode = Just (question4 ^. uuid)
        let expected = [q4_it1_question6 ^. uuid, q4_it1_q6_answerYes ^. uuid, q4_it1_q6_aYes_followUpQuestion4 ^. uuid]
        -- WHEN:
        let result = takeDiffSuffix sharedNode path
        -- THEN:
        result `shouldBe` expected
    describe "shouldWeMigrate" $ do
      it "Yes" $
        -- GIVEN:
       do
        let pPath = [q4_it1_question6 ^. uuid, q4_it1_q6_answerNo ^. uuid]
        let tPath = []
        let expected = True
        -- WHEN:
        let result = shouldWeMigrate km1WithQ4 pPath tPath
        -- THEN:
        result `shouldBe` expected
      it "No" $
        -- GIVEN:
       do
        let pPath = [q4_it1_question6 ^. uuid, q4_it1_q6_answerYes ^. uuid, q4_it1_q6_aYes_followUpQuestion4 ^. uuid]
        let tPath = []
        let expected = False
        -- WHEN:
        let result = shouldWeMigrate km1WithQ4 pPath tPath
        -- THEN:
        result `shouldBe` expected
    describe "computeDesiredPath" $ do
      it "m_km1_ch1_q1__to_ch2_q3_aNo" $
        -- GIVEN:
       do
        let pPathSuffix = [chapter1 ^. uuid, question1 ^. uuid]
        let tPathSuffix = [chapter2 ^. uuid, question3 ^. uuid, q3_answerNo ^. uuid, question1 ^. uuid]
        let replies = [rQ1, rQ2]
        -- AND: Make expectation
        let expRQ1 = rQ1 & path .~ (i' . s') tPathSuffix
        let expected = [expRQ1, rQ2]
        -- WHEN:
        let result = computeDesiredPath pPathSuffix tPathSuffix replies
        -- THEN:
        result `shouldBe` expected
      it "m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo" $
        -- GIVEN:
       do
        let pPathSuffix = [q4_it1_question5 ^. uuid]
        let tPathSuffix = [q4_it1_question6 ^. uuid, q4_it1_q6_answerNo ^. uuid, q4_it1_question5 ^. uuid]
        let replies =
              [ rQ3
              , rQ4
              , rQ4_it1_q5
              , rQ4_it1_q5_it1_question7
              , rQ4_it1_q5_it1_question8
              , rQ4_it1_q6
              , rQ4_it2_q5
              , rQ4_it2_q6
              ]
        -- AND: Make expectation
        let ch2___q4_0 = s' [chapter2 ^. uuid, question4 ^. uuid] ++ ["0"] ++ s' tPathSuffix
        let ch2___q4_1 = s' [chapter2 ^. uuid, question4 ^. uuid] ++ ["1"] ++ s' tPathSuffix
        let expRQ4_it1_q5 = rQ4_it1_q5 & path .~ i' ch2___q4_0
        let expRQ4_it1_q5_it1_question7 =
              rQ4_it1_q5_it1_question7 & path .~ i' (ch2___q4_0 ++ ["0"] ++ s' [q4_it1_q5_it2_question7 ^. uuid])
        let expRQ4_it1_q5_it1_question8 =
              rQ4_it1_q5_it1_question8 & path .~ i' (ch2___q4_0 ++ ["0"] ++ s' [q4_it1_q5_it2_question8 ^. uuid])
        let expRQ4_it2_q5 = rQ4_it2_q5 & path .~ i' ch2___q4_1
        let expected =
              [ rQ3
              , rQ4
              , expRQ4_it1_q5
              , expRQ4_it1_q5_it1_question7
              , expRQ4_it1_q5_it1_question8
              , rQ4_it1_q6
              , expRQ4_it2_q5
              , rQ4_it2_q6
              ]
        -- WHEN:
        let result = computeDesiredPath pPathSuffix tPathSuffix replies
        -- THEN:
        result `shouldBe` expected
    describe "deleteUnwantedReplies" $
      it "question5" $
        -- GIVEN:
       do
        let eUuid = q4_it1_question5 ^. uuid
        let replies = fReplies
        -- AND: Make expectation
        let expected = [rQ1, rQ2, rQ2_aYes_fuQ1, rQ3, rQ4, rQ4_it1_q6, rQ4_it2_q6, rQ9, rQ10]
        -- WHEN:
        let result = deleteUnwantedReplies eUuid replies
        -- THEN:
        result `shouldBe` expected
    describe "sanitize" $ do
      createSanitizeRepliesWithEventsTest
        "MoveQuestionEvent should adjust paths of affected answers if it moves question just in root of KM"
        (MoveQuestionEvent' m_km1_ch1_q1__to_ch2_q3_aNo)
        True
      createSanitizeRepliesWithEventsTest
        "MoveQuestionEvent should adjust paths of affected answers if it moves question within one ListQuestion context"
        (MoveQuestionEvent' m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo)
        True
      createSanitizeRepliesWithEventsTest
        "MoveQuestionEvent should delete affected answers if it moves question outside one ListQuestion context"
        (MoveQuestionEvent' m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4)
        False
      createSanitizeRepliesWithEventsTest
        "MoveAnswerEvent should delete affected answers"
        (MoveAnswerEvent' m_km1_ch1_q2_aYes__to_ch2_q3)
        True
      createSanitizeRepliesWithEventsTest
        "MoveExpertEvent has no effect on replies"
        (MoveExpertEvent' m_km1_ch1_q2_eAlbert__to_ch2_q3)
        False
      createSanitizeRepliesWithEventsTest
        "MoveReferenceEvent has no effect on replies"
        (MoveReferenceEvent' m_km1_ch1_q2_r1__to_ch2_q3)
        False

-- --------------------------------
-- TEST TEMPLATES
-- --------------------------------
createComputeParentPathTest uuid path =
  it ("computeParentPath for '" ++ U.toString uuid ++ "'") $
    -- GIVEN:
   do
    let parentMap = makeParentMap km1WithQ4
    let expected = path
    -- WHEN:
    let result = computeParentPath parentMap uuid
    -- THEN:
    result `shouldBe` expected

createSanitizeRepliesWithEventsTest name event shouldNotEqual =
  it name $
    -- GIVEN:
   do
    let events = [event]
    -- AND:
    let expected = fReplies
    -- WHEN:
    let result = sanitizeRepliesWithEvents km1WithQ4 fReplies events
    -- THEN:
    if not shouldNotEqual
      then result `shouldBe` expected
      else result `shouldNotBe` expected

-- --------------------------------
-- DATA
-- --------------------------------
q4_it1_q6_aYes_followUpQuestion4_path =
  [ chapter2 ^. uuid
  , question4 ^. uuid
  , q4_it1_question6 ^. uuid
  , q4_it1_q6_answerYes ^. uuid
  , q4_it1_q6_aYes_followUpQuestion4 ^. uuid
  ]
