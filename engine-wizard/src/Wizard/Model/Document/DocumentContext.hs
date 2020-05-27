module Wizard.Model.Document.DocumentContext where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Model.Config.AppConfig
import Wizard.Model.Level.Level
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Model.User.User

data DocumentFormat
  = JSON
  | HTML
  | PDF
  | LaTeX
  | Docx
  | ODT
  | Markdown
  | RTF
  | RST
  | AsciiDoc
  | DokuWiki
  | MediaWiki
  | EPUB2
  | EPUB3
  deriving (Show, Eq, Enum, Bounded, Generic)

data DocumentContext =
  DocumentContext
    { _documentContextUuid :: U.UUID
    , _documentContextConfig :: DocumentContextConfig
    , _documentContextQuestionnaireUuid :: String
    , _documentContextQuestionnaireName :: String
    , _documentContextQuestionnaireReplies :: [Reply]
    , _documentContextLevel :: Int
    , _documentContextKnowledgeModel :: KnowledgeModel
    , _documentContextMetrics :: [Metric]
    , _documentContextLevels :: [Level]
    , _documentContextReport :: Report
    , _documentContextPackage :: Package
    , _documentContextOrganization :: AppConfigOrganization
    , _documentContextCreatedBy :: Maybe User
    , _documentContextCreatedAt :: UTCTime
    , _documentContextUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DocumentContext where
  a == b =
    _documentContextUuid a == _documentContextUuid b &&
    _documentContextConfig a == _documentContextConfig b &&
    _documentContextQuestionnaireUuid a == _documentContextQuestionnaireUuid b &&
    _documentContextQuestionnaireName a == _documentContextQuestionnaireName b &&
    _documentContextLevel a == _documentContextLevel b &&
    _documentContextKnowledgeModel a == _documentContextKnowledgeModel b &&
    _documentContextMetrics a == _documentContextMetrics b &&
    _documentContextLevels a == _documentContextLevels b &&
    _documentContextReport a == _documentContextReport b &&
    _documentContextPackage a == _documentContextPackage b &&
    _documentContextOrganization a == _documentContextOrganization b &&
    _documentContextCreatedBy a == _documentContextCreatedBy b

data DocumentContextConfig =
  DocumentContextConfig
    { _documentContextConfigLevelsEnabled :: Bool
    , _documentContextConfigClientUrl :: String
    }
  deriving (Show, Eq, Generic)
