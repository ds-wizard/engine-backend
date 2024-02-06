module Wizard.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.List as L

import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Registry.RegistryTemplate
import WizardLib.Common.Util.Coordinate
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

computeDocumentTemplateState :: [RegistryTemplate] -> DocumentTemplate -> DocumentTemplateState
computeDocumentTemplateState tmlsFromRegistry tml =
  if not (isDocumentTemplateSupported False tml)
    then UnsupportedMetamodelVersionDocumentTemplateState
    else DefaultDocumentTemplateState

computeDocumentTemplateState' :: DocumentTemplateList -> DocumentTemplateState
computeDocumentTemplateState' tml
  | not (isDocumentTemplateSupported False tml) = UnsupportedMetamodelVersionDocumentTemplateState
  | otherwise = DefaultDocumentTemplateState

selectDocumentTemplateByOrgIdAndTmlId tml =
  L.find (\t -> t.organizationId == tml.organizationId && t.templateId == tml.templateId)

selectOrganizationByOrgId tml = L.find (\org -> org.organizationId == tml.organizationId)

getUsablePackagesForDocumentTemplate tml = chooseTheNewest . groupPackages . filterPackages tml
  where
    filterPackages tml = filter (\pkg -> not . null $ filterDocumentTemplates (Just pkg.pId) [tml])

isDocumentTemplateSupported True tml = True
isDocumentTemplateSupported False tml = tml.metamodelVersion == documentTemplateMetamodelVersion

isDocumentTemplateInPhase (Just phase) tml = tml.phase == phase
isDocumentTemplateInPhase _ _ = True

filterDocumentTemplates mPkgId tmls =
  case mPkgId of
    Just pkgId -> filter (filterDocumentTemplate . splitCoordinate $ pkgId) tmls
    Nothing -> tmls
  where
    filterDocumentTemplate pkgIdSplit template = fitsIntoKMSpecs pkgIdSplit template.allowedPackages
