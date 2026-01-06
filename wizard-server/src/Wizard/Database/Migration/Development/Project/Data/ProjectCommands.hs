module Wizard.Database.Migration.Development.Project.Data.ProjectCommands where

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User
import WizardLib.Public.Model.PersistentCommand.Project.CreateProjectCommand

command1 :: CreateProjectCommand
command1 =
  CreateProjectCommand
    { name = "Project 1"
    , emails =
        [ userAlbert.email
        , userNikola.email
        ]
    , knowledgeModelPackageId = netherlandsKmPackageV2.pId
    , documentTemplateId = Just wizardDocumentTemplate.tId
    }

command2 :: CreateProjectCommand
command2 =
  CreateProjectCommand
    { name = "Project 2"
    , emails =
        [ userAlbert.email
        , userIsaac.email
        ]
    , knowledgeModelPackageId = netherlandsKmPackageV2.pId
    , documentTemplateId = Nothing
    }
