module Wizard.Model.Document.DocumentHelpers where

import qualified Data.Char as C

import Wizard.Model.Document.DocumentContext

formatExtension :: DocumentFormat -> String
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

allFormats :: [DocumentFormat]
allFormats = [minBound .. maxBound]

stringFormatMap :: [(String, DocumentFormat)]
stringFormatMap = [(map C.toLower . show $ format, format) | format <- allFormats]

stringToFormat :: String -> Maybe DocumentFormat
stringToFormat str = lookup (C.toLower <$> str) stringFormatMap

instance Read DocumentFormat where
  readsPrec _ str =
    case stringToFormat str of
      Just format -> [(format, "")]
      _ -> []
