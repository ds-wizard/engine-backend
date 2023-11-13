module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireCommands where

import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.Public.Model.PersistentCommand.Questionnaire.CreateQuestionnaireCommand

command1 :: CreateQuestionnaireCommand
command1 =
  CreateQuestionnaireCommand
    { name = "Questionnaire 1"
    , emails =
        [ userAlbert.email
        , userNikola.email
        ]
    , packageId = netherlandsPackageV2.pId
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
    , packageId = netherlandsPackageV2.pId
    , documentTemplateId = Nothing
    }
