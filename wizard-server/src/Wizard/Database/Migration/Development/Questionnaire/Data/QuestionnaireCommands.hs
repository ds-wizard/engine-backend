module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireCommands where

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import WizardLib.Public.Model.PersistentCommand.Questionnaire.CreateQuestionnaireCommand

command1 :: CreateQuestionnaireCommand
command1 =
  CreateQuestionnaireCommand
    { name = "Questionnaire 1"
    , emails =
        [ userAlbert.email
        , userNikola.email
        ]
    , knowledgeModelPackageId = netherlandsKmPackageV2.pId
    , documentTemplateId = Just wizardDocumentTemplate.tId
    }

command2 :: CreateQuestionnaireCommand
command2 =
  CreateQuestionnaireCommand
    { name = "Questionnaire 2"
    , emails =
        [ userAlbert.email
        , userIsaac.email
        ]
    , knowledgeModelPackageId = netherlandsKmPackageV2.pId
    , documentTemplateId = Nothing
    }
