module Service.Template.TemplateMapper where

import qualified Text.FromHTML as FromHTML

import Localization
import Model.DataManagementPlan.DataManagementPlan
import Model.Error.Error

formatToToHTMLType :: DataManagementPlanFormat -> Maybe FromHTML.ExportType
formatToToHTMLType HTML = Just FromHTML.HTML
formatToToHTMLType LaTeX = Just FromHTML.LaTeX
formatToToHTMLType Markdown = Just FromHTML.Markdown
formatToToHTMLType Docx = Just FromHTML.Docx
formatToToHTMLType ODT = Just FromHTML.ODT
formatToToHTMLType PDF = Just FromHTML.PDF
formatToToHTMLType RTF = Just FromHTML.RTF
formatToToHTMLType RST = Just FromHTML.RST
formatToToHTMLType AsciiDoc = Just FromHTML.AsciiDoc
formatToToHTMLType DokuWiki = Just FromHTML.DokuWiki
formatToToHTMLType MediaWiki = Just FromHTML.MediaWiki
formatToToHTMLType EPUB2 = Just FromHTML.EPUB2
formatToToHTMLType EPUB3 = Just FromHTML.EPUB3
formatToToHTMLType _ = Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heFormatToToHTMLType format callback =
  case formatToToHTMLType format of
    Just toHTMLType -> callback toHTMLType
    Nothing -> return $ Left . GeneralServerError $ _ERROR_SERVICE_TEMPLATE__UKNOWN_FORMAT
