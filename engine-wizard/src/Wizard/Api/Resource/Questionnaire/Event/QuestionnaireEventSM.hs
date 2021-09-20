module Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

instance ToSchema QuestionnaireEventDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventDTO where
  declareNamedSchema = simpleToSchema' "_setReplyEventDTO" (toSetReplyEventDTO sre_rQ1 (Just userAlbert))

instance ToSchema ClearReplyEventDTO where
  declareNamedSchema = simpleToSchema' "_clearReplyEventDTO" (toClearReplyEventDTO cre_rQ1 (Just userAlbert))

instance ToSchema SetPhaseEventDTO where
  declareNamedSchema = simpleToSchema' "_setPhaseEventDTO" (toSetPhaseEventDTO sphse_1 (Just userAlbert))

instance ToSchema SetLabelsEventDTO where
  declareNamedSchema = simpleToSchema' "_setLabelsEventDTO" (toSetLabelsEventDTO slble_rQ2 (Just userAlbert))

instance ToSchema ResolveCommentThreadEventDTO where
  declareNamedSchema =
    simpleToSchema' "_resolveCommentThreadEventDTO" (toResolveCommentThreadEventDTO rte_rQ1_t1 (Just userAlbert))

instance ToSchema ReopenCommentThreadEventDTO where
  declareNamedSchema =
    simpleToSchema' "_reopenCommentThreadEventDTO" (toReopenCommentThreadEventDTO ote_rQ1_t1 (Just userAlbert))

instance ToSchema DeleteCommentThreadEventDTO where
  declareNamedSchema =
    simpleToSchema' "_deleteCommentThreadEventDTO" (toDeleteCommentThreadEventDTO dte_rQ1_t1 (Just userAlbert))

instance ToSchema AddCommentEventDTO where
  declareNamedSchema = simpleToSchema' "_addCommentEventDTO" (toAddCommentEventDTO ace_rQ1_t1_1 (Just userAlbert))

instance ToSchema EditCommentEventDTO where
  declareNamedSchema = simpleToSchema' "_editCommentEventDTO" (toEditCommentEventDTO ece_rQ1_t1_1 (Just userAlbert))

instance ToSchema DeleteCommentEventDTO where
  declareNamedSchema = simpleToSchema' "_deleteCommentEventDTO" (toDeleteCommentEventDTO dce_rQ1_t1_1 (Just userAlbert))
