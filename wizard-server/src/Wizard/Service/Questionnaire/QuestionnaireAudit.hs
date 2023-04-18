module Wizard.Service.Questionnaire.QuestionnaireAudit where

import qualified Data.UUID as U

import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditQuestionnaireListEvents :: U.UUID -> AppContextM ()
auditQuestionnaireListEvents qtnUuid = logAudit "questionnaire" "listEvents" (U.toString qtnUuid)

auditQuestionnaireDetailEvent :: U.UUID -> AppContextM ()
auditQuestionnaireDetailEvent qtnUuid = logAudit "questionnaire" "detailEvent" (U.toString qtnUuid)
