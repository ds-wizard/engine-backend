module Wizard.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.List as L

import Shared.Coordinate.Util.Coordinate
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Registry.RegistryTemplate

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

getUsableKnowledgeModelPackagesForDocumentTemplate tml = chooseTheNewest . groupPackages . filterPackages tml
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

isPkgAllowedByDocumentTemplate :: String -> DocumentTemplate -> Bool
isPkgAllowedByDocumentTemplate pkgId template =
  fitsIntoKMSpecs pkgIdSplit template.allowedPackages
  where
    pkgIdSplit = splitCoordinate pkgId
