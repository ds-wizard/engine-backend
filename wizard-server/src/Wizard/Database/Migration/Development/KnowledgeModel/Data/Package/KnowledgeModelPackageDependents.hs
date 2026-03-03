module Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents where

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Wizard.Service.Project.ProjectMapper

netherlandsKmPackageDeletionImpact :: KnowledgeModelPackageDeletionImpact
netherlandsKmPackageDeletionImpact =
  KnowledgeModelPackageDeletionImpact
    { uuid = netherlandsKmPackage.uuid
    , name = netherlandsKmPackage.name
    , version = netherlandsKmPackage.version
    , packages = [netherlandsKmPackageReference]
    , editors = [amsterdamKnowledgeModelEditorSuggestion]
    , projects = [toSimple project4]
    }

netherlandsKmPackageReference :: KnowledgeModelPackageReference
netherlandsKmPackageReference =
  KnowledgeModelPackageReference
    { uuid = netherlandsKmPackageV2.uuid
    , name = netherlandsKmPackageV2.name
    , version = netherlandsKmPackageV2.version
    }

netherlandsKmPackageV2DeletionImpact :: KnowledgeModelPackageDeletionImpact
netherlandsKmPackageV2DeletionImpact =
  KnowledgeModelPackageDeletionImpact
    { uuid = netherlandsKmPackageV2.uuid
    , name = netherlandsKmPackageV2.name
    , version = netherlandsKmPackageV2.version
    , packages = []
    , editors = []
    , projects = []
    }
