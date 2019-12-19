module Wizard.Specs.Service.Migration.Questionnaire.SanitizatorSpec where

import Control.Lens ((&), (.~), (^.))
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Wizard.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.LensesConfig
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.Migration.Questionnaire.Sanitizator

sanitizatorSpec =
  describe "Sanatizor" $
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    -- Questions
    -- -------------------------------------------------------------
   do
    describe "sanitize question" $ do
      createTest "OptionsQuestion" "AnswerReply" "Keep" rQ2 (rQ2 ^. value) (Just rQ2)
      createTest "OptionsQuestion" "non-existing AnswerReply" "Removed" rQ2 nonExistingAnswerReply Nothing
      createTest "OptionsQuestion" "StringReply" "Removed" rQ2 stringReply Nothing
      createTest "OptionsQuestion" "ItemListReply" "Removed" rQ2 itemListReply Nothing
      createTest "OptionsQuestion" "IntegrationReply (plain)" "Removed" rQ2 plainIntegrationReply Nothing
      createTest "OptionsQuestion" "IntegrationReply (full)" "Removed" rQ2 fullIntegrationReply Nothing
      -- -------------------------------------------------------------
      createTest "ListQuestion" "AnswerReply" "Removed" rQ4 answerReply Nothing
      createTest "ListQuestion" "StringReply" "Removed" rQ4 stringReply Nothing
      createTest "ListQuestion" "ItemListReply" "Keep" rQ4 (rQ4 ^. value) (Just rQ4)
      createTest "ListQuestion" "IntegrationReply (plain)" "Removed" rQ4 plainIntegrationReply Nothing
      createTest "ListQuestion" "IntegrationReply (full)" "Removed" rQ4 fullIntegrationReply Nothing
      -- -------------------------------------------------------------
      createTest "ValueQuestion" "AnswerReply" "Removed" rQ1 answerReply Nothing
      createTest "ValueQuestion" "StringReply" "Keep" rQ1 (rQ1 ^. value) (Just rQ1)
      createTest "ValueQuestion" "ItemListReply" "Removed" rQ1 itemListReply Nothing
      createTest "ValueQuestion" "IntegrationReply (plain)" "Keep" rQ1 plainIntegrationReply (Just rQ1)
      createTest "ValueQuestion" "IntegrationReply (full)" "Keep" rQ1 fullIntegrationReply (Just rQ1)
      -- -------------------------------------------------------------
      createTest "IntegrationQuestion" "AnswerReply" "Removed" rQ9 answerReply Nothing
      createTest "IntegrationQuestion" "StringReply" "Keep" rQ9 stringReply (Just rQ9)
      createTest "IntegrationQuestion" "ItemListReply" "Removed" rQ9 itemListReply Nothing
      createTest "IntegrationQuestion" "IntegrationReply (plain)" "Keep" rQ9 (rQ9 ^. value) (Just rQ9)
      createTest "IntegrationQuestion" "IntegrationReply (full)" "Keep" rQ9 (rQ9 ^. value) (Just rQ9)

-- --------------------------------
-- TEST TEMPLATE
-- --------------------------------
createTest qType replyType result reply replyValue expected =
  it (qType ++ ": " ++ replyType ++ " -> " ++ result) $
    -- Given:
   do
    let inReply = reply & value .~ replyValue
    -- When:
    let result = sanitizeReply km1WithQ4 inReply
    -- Then:
    result `shouldBe` expected

-- --------------------------------
-- DATA
-- --------------------------------
answerReply = AnswerReply $ q2_answerYes ^. uuid

nonExistingAnswerReply = AnswerReply . fromJust . U.fromString $ "5c4141ac-4a61-492f-b07f-467932753f0a"

stringReply = StringReply "Plain reply to 9st question"

itemListReply = ItemListReply 2

plainIntegrationReply = IntegrationReply {_integrationReplyValue = PlainValue "Reply to 1st question"}

fullIntegrationReply =
  IntegrationReply
    { _integrationReplyValue =
        IntegrationValue {_integrationValueIntId = "", _integrationValueIntValue = "Reply to 1st question"}
    }
