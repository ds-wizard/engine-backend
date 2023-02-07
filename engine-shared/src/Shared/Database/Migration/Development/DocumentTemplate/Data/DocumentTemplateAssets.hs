module Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Time

import Shared.Constant.App
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.Uuid

assetLogo :: DocumentTemplateAsset
assetLogo =
  DocumentTemplateAsset
    { documentTemplateId = wizardDocumentTemplate.tId
    , uuid = u' "6c367648-9b60-4307-93b2-0851938adee0"
    , fileName = "text.txt"
    , contentType = "text/plain"
    , fileSize = 5 * 1024
    , appUuid = defaultAppUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

assetLogoEdited :: DocumentTemplateAsset
assetLogoEdited =
  assetLogo
    { fileName = "edited-text.txt"
    }

assetLogoContent :: BS.ByteString
assetLogoContent =
  BS.pack $
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam auctor pellentesque velit, sollicitudin euismod "
      ++ "arcu varius a. Pellentesque consectetur a felis nec finibus. Curabitur at porttitor turpis. Vivamus eu imperdiet "
      ++ "massa. Fusce vitae dolor et nulla vulputate condimentum. Aenean tincidunt, magna quis viverra porta, nulla "
      ++ "mauris semper nibh, ac interdum quam orci at elit. Donec aliquet tempor erat, sed consectetur sapien eleifend "
      ++ "id. Nullam sagittis justo a lobortis fermentum. Nunc pretium sem sed lectus lacinia, et tempus nulla suscipit. "
      ++ "Aliquam volutpat molestie nibh sit amet iaculis."
