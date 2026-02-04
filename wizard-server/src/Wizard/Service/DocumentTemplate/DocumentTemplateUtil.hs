module Wizard.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.List as L

import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Registry.RegistryTemplate

computeDocumentTemplateState :: [RegistryTemplate] -> DocumentTemplate -> DocumentTemplateState
computeDocumentTemplateState tmlsFromRegistry tml =
  if not (isDocumentTemplateSupported tml.metamodelVersion)
    then UnsupportedMetamodelVersionDocumentTemplateState
    else DefaultDocumentTemplateState

computeDocumentTemplateState' :: DocumentTemplateList -> DocumentTemplateState
computeDocumentTemplateState' tml
  | not (isDocumentTemplateSupported tml.metamodelVersion) = UnsupportedMetamodelVersionDocumentTemplateState
  | otherwise = DefaultDocumentTemplateState

selectDocumentTemplateByOrgIdAndTmlId tml =
  L.find (\t -> t.organizationId == tml.organizationId && t.templateId == tml.templateId)

selectOrganizationByOrgId tml = L.find (\org -> org.organizationId == tml.organizationId)

isDocumentTemplateInPhase (Just phase) tml = tml.phase == phase
isDocumentTemplateInPhase _ _ = True

filterDocumentTemplates mCoordinate tmls =
  case mCoordinate of
    Just coordinate -> filter (filterDocumentTemplate coordinate) tmls
    Nothing -> tmls
  where
    filterDocumentTemplate coordinate template = fitsIntoKMSpecs coordinate template.allowedPackages
