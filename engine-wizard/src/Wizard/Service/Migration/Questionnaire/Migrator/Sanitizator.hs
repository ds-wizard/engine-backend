module Wizard.Service.Migration.Questionnaire.Migrator.Sanitizator where

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Questionnaire.QuestionnaireReply
import qualified Wizard.Service.Migration.Questionnaire.Migrator.ChangeQTypeSanitizator as CTS
import qualified Wizard.Service.Migration.Questionnaire.Migrator.MoveSanitizator as MS

sanitizeReplies :: KnowledgeModel -> KnowledgeModel -> [Reply] -> [Reply]
sanitizeReplies oldKm newKm = MS.sanitizeReplies oldKm newKm . CTS.sanitizeReplies newKm
