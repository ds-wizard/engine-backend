module Shared.DocumentTemplate.Constant.DocumentTemplate where

import Shared.Common.Model.Common.SemVer2Tuple

documentTemplateMetamodelVersion :: SemVer2Tuple
documentTemplateMetamodelVersion = SemVer2Tuple 17 0

isDocumentTemplateSupported :: SemVer2Tuple -> Bool
isDocumentTemplateSupported metamodelVersion
  | metamodelVersion == documentTemplateMetamodelVersion = True
  | metamodelVersion.major == documentTemplateMetamodelVersion.major && metamodelVersion.minor < documentTemplateMetamodelVersion.minor = True
  | otherwise = False

isDocumentTemplateUnsupported :: SemVer2Tuple -> Bool
isDocumentTemplateUnsupported = not . isDocumentTemplateSupported
