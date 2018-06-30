module Model.DataManagementPlan.DataManagementPlanHelpers where

import qualified Data.Char as C

import Model.DataManagementPlan.DataManagementPlan

formatExtension :: DataManagementPlanFormat -> String
formatExtension JSON = ".json"
formatExtension HTML = ".html"
formatExtension LaTeX = ".tex"
formatExtension Markdown = ".md"
formatExtension Docx = ".docx"
formatExtension ODT = ".odt"
formatExtension PDF = ".pdf"
formatExtension RTF = ".rtf"
formatExtension RST = ".rst"
formatExtension AsciiDoc = ".adoc"
formatExtension DokuWiki = ".dw.txt"
formatExtension MediaWiki = ".mw.txt"
formatExtension EPUB2 = ".epub"
formatExtension EPUB3 = ".epub"

allFormats :: [DataManagementPlanFormat]
allFormats = [minBound .. maxBound]

stringFormatMap :: [(String, DataManagementPlanFormat)]
stringFormatMap = [(map C.toLower . show $ format, format) | format <- allFormats]

stringToFormat :: String -> Maybe DataManagementPlanFormat
stringToFormat str = lookup (C.toLower <$> str) stringFormatMap

instance Read DataManagementPlanFormat where
  readsPrec _ str =
    case stringToFormat str of
      Just format -> [(format, "")]
      _ -> []
