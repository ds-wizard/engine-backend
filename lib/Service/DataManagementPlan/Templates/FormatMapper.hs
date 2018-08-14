module Service.DataManagementPlan.Templates.FormatMapper where

import Data.Default
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.FromHTML as FromHTML

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Localization
import Model.DataManagementPlan.DataManagementPlan
import Model.Error.Error
import Service.DataManagementPlan.Templates.Html

-- | Enumeration of supported export document types
type DMPExportType = FromHTML.ExportType

strToBSL :: String -> BSL.ByteString
strToBSL = BSL.fromStrict . E.encodeUtf8 . T.pack

bsToStr :: BS.ByteString -> String
bsToStr = T.unpack . E.decodeUtf8

toHTML :: DataManagementPlanDTO -> BSL.ByteString
toHTML = strToBSL . mkHTMLString

toFormat :: DataManagementPlanFormat -> DataManagementPlanDTO -> Either AppError BSL.ByteString
toFormat format = toType (formatToType format)
  where
    toType :: Maybe DMPExportType -> DataManagementPlanDTO -> Either AppError BSL.ByteString
    toType (Just eType) dmp = handleResult . FromHTML.fromHTML eType . mkHTMLString $ dmp
    toType _ _ = Left . GeneralServerError $ _ERROR_SERVICE_DMP__UKNOWN_FORMAT
    handleResult :: Either BS.ByteString BS.ByteString -> Either AppError BSL.ByteString
    handleResult (Right result) = Right . BSL.fromStrict $ result
    handleResult (Left err) = Left . GeneralServerError $ _ERROR_SERVICE_DMP__TRANSFORMATION_FAILED (bsToStr err)

mkHTMLString :: DataManagementPlanDTO -> String
mkHTMLString = dmp2html def

formatToType :: DataManagementPlanFormat -> Maybe DMPExportType
formatToType HTML = Just FromHTML.HTML
formatToType LaTeX = Just FromHTML.LaTeX
formatToType Markdown = Just FromHTML.Markdown
formatToType Docx = Just FromHTML.Docx
formatToType ODT = Just FromHTML.ODT
formatToType PDF = Just FromHTML.PDF
formatToType RTF = Just FromHTML.RTF
formatToType RST = Just FromHTML.RST
formatToType AsciiDoc = Just FromHTML.AsciiDoc
formatToType DokuWiki = Just FromHTML.DokuWiki
formatToType MediaWiki = Just FromHTML.MediaWiki
formatToType EPUB2 = Just FromHTML.EPUB2
formatToType EPUB3 = Just FromHTML.EPUB3
formatToType _ = Nothing
