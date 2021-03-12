module Wizard.Specs.Service.Migration.Questionnaire.ChangeQTypeSanitizatorSpec where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.Migration.Questionnaire.Migrator.ChangeQTypeSanitizator
import qualified Wizard.Service.User.UserMapper as UM

sanitizatorSpec =
  describe "ChangeQTypeSanatizor" $
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    -- Questions
    -- -------------------------------------------------------------
  describe "sanitize question" $ do
    createTest "OptionsQuestion" "AnswerReply" "Keep" rQ2 (snd rQ2) (Just rQ2)
    createTest "OptionsQuestion" "non-existing AnswerReply" "Removed" rQ2 nonExistingAnswerReply Nothing
    createTest "OptionsQuestion" "MultiChoiceReply" "Removed" rQ2 multiChoiceReply Nothing
    createTest "OptionsQuestion" "StringReply" "Removed" rQ2 stringReply Nothing
    createTest "OptionsQuestion" "ItemListReply" "Removed" rQ2 itemListReply Nothing
    createTest "OptionsQuestion" "IntegrationReply (plain)" "Removed" rQ2 plainIntegrationReply Nothing
    createTest "OptionsQuestion" "IntegrationReply (full)" "Removed" rQ2 fullIntegrationReply Nothing
      -- -------------------------------------------------------------
    createTest "MultiChoiceQuestion" "AnswerReply" "Removed" rQ11 (snd rQ2) Nothing
    createTest "MultiChoiceQuestion" "MultiChoiceReply" "Keep" rQ11 multiChoiceReply (Just rQ11)
    createTest "MultiChoiceQuestion" "StringReply" "Removed" rQ11 stringReply Nothing
    createTest "MultiChoiceQuestion" "ItemListReply" "Removed" rQ11 itemListReply Nothing
    createTest "MultiChoiceQuestion" "IntegrationReply (plain)" "Removed" rQ11 plainIntegrationReply Nothing
    createTest "MultiChoiceQuestion" "IntegrationReply (full)" "Removed" rQ11 fullIntegrationReply Nothing
      -- -------------------------------------------------------------
    createTest "ListQuestion" "AnswerReply" "Removed" rQ4 answerReply Nothing
    createTest "ListQuestion" "MultiChoiceReply" "Removed" rQ4 multiChoiceReply Nothing
    createTest "ListQuestion" "StringReply" "Removed" rQ4 stringReply Nothing
    createTest "ListQuestion" "ItemListReply" "Keep" rQ4 (snd rQ4) (Just rQ4)
    createTest "ListQuestion" "IntegrationReply (plain)" "Removed" rQ4 plainIntegrationReply Nothing
    createTest "ListQuestion" "IntegrationReply (full)" "Removed" rQ4 fullIntegrationReply Nothing
      -- -------------------------------------------------------------
    createTest "ValueQuestion" "AnswerReply" "Removed" rQ1 answerReply Nothing
    createTest "ValueQuestion" "MultiChoiceReply" "Removed" rQ1 multiChoiceReply Nothing
    createTest "ValueQuestion" "StringReply" "Keep" rQ1 (snd rQ1) (Just rQ1)
    createTest "ValueQuestion" "ItemListReply" "Removed" rQ1 itemListReply Nothing
    createTest "ValueQuestion" "IntegrationReply (plain)" "Keep" rQ1 plainIntegrationReply (Just rQ1)
    createTest "ValueQuestion" "IntegrationReply (full)" "Keep" rQ1 fullIntegrationReply (Just rQ1)
      -- -------------------------------------------------------------
    createTest "IntegrationQuestion" "AnswerReply" "Removed" rQ9 answerReply Nothing
    createTest "IntegrationQuestion" "MultiChoiceReply" "Removed" rQ9 multiChoiceReply Nothing
    createTest "IntegrationQuestion" "StringReply" "Keep" rQ9 stringReply (Just rQ9)
    createTest "IntegrationQuestion" "ItemListReply" "Removed" rQ9 itemListReply Nothing
    createTest "IntegrationQuestion" "IntegrationReply (plain)" "Keep" rQ9 (snd rQ9) (Just rQ9)
    createTest "IntegrationQuestion" "IntegrationReply (full)" "Keep" rQ9 (snd rQ9) (Just rQ9)

-- --------------------------------
-- TEST TEMPLATE
-- --------------------------------
createTest qType replyType result (path, value) newValue expected =
  it (qType ++ ": " ++ replyType ++ " -> " ++ result) $
    -- Given:
   do
    let inReply = (path, newValue)
    -- When:
    let result = sanitizeReply km1WithQ4 inReply
    -- Then:
    result `shouldBe` expected

-- --------------------------------
-- DATA
-- --------------------------------
answerReply = createReply $ AnswerReply {_answerReplyValue = q2_answerYes ^. uuid}

nonExistingAnswerReply = createReply $ AnswerReply {_answerReplyValue = u' "5c4141ac-4a61-492f-b07f-467932753f0a"}

multiChoiceReply =
  createReply $
  MultiChoiceReply
    {_multiChoiceReplyValue = [u' "24b6d097-1be1-44d1-95bd-e7cf50052093", u' "5db38ad4-7bad-4cc7-84e6-3270b8556593"]}

stringReply = createReply $ StringReply {_stringReplyValue = "Plain reply to 9st question"}

itemListReply =
  createReply $
  ItemListReply
    {_itemListReplyValue = [u' "58c1379d-8b1d-4d88-a890-10b4244ab7bd", u' "2f089514-54a6-41a2-9cbd-ba0e0551fc77"]}

plainIntegrationReply = createReply $ IntegrationReply {_integrationReplyValue = PlainType "Reply to 1st question"}

fullIntegrationReply =
  createReply $
  IntegrationReply
    { _integrationReplyValue =
        IntegrationType {_integrationTypeIntId = "", _integrationTypeValue = "Reply to 1st question"}
    }

createReply :: ReplyValue -> Reply
createReply value =
  Reply
    { _replyValue = value
    , _replyCreatedBy = Just . UM.toSuggestionDTO . UM.toSuggestion $ userAlbert
    , _replyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
