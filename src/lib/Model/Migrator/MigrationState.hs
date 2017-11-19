module Model.Migrator.MigrationState where

import Control.Lens ((^.), makeLenses, (&), (.~))
import qualified Data.UUID as U

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data MigrationStatus
  = MSCreated
  | MSRunning
  | MSError
  | MSCompleted
  deriving (Show, Eq)

data MigrationError
  = NoError
  | ApplicatorError String
  deriving (Show, Eq)

type DiffTableRecord = (U.UUID, Event)

type DiffTable = [DiffTableRecord]

data DiffTree = Leaf U.UUID Bool | Node U.UUID Bool [DiffTree] deriving (Show, Eq)

data MigrationState = MigrationState
  { _msStatus :: MigrationStatus
  , _msParentPackageId :: String
  , _msLocalizationPackageId :: String
  , _msCurrentKnowledgeModel :: Maybe KnowledgeModel
  , _msError :: MigrationError
  , _msParentEvents :: [Event]
  , _msLocalizationEvents :: [Event]
  , _msDiffTable :: DiffTable
  , _msDiffTree :: Maybe DiffTree
  } deriving (Show, Eq)

makeLenses ''MigrationState

buildDiffTable :: [Event] -> DiffTable
buildDiffTable events = (\event -> (getTargetUuid event, event)) <$> events

class BuildDiffTree a where
  buildDiffTree :: a -> DiffTree

instance BuildDiffTree KnowledgeModel where
  buildDiffTree km = Node (km ^. kmUuid) True (buildDiffTree <$> (km ^. kmChapters))

instance BuildDiffTree Chapter where
  buildDiffTree ch = Node (ch ^. chUuid) True (buildDiffTree <$> (ch ^. chQuestions))

instance BuildDiffTree Question where
  buildDiffTree q = Node (q ^. qUuid) True ((buildDiffTree <$> (q ^. qAnswers)) ++ (buildDiffTree <$> (q ^. qReferences)) ++ (buildDiffTree <$> (q ^. qExperts)))

instance BuildDiffTree Answer where
  buildDiffTree ans = Node (ans ^. ansUuid) True (buildDiffTree <$> (ans ^. ansFollowing))

instance BuildDiffTree Expert where
  buildDiffTree exp = Leaf (exp ^. expUuid) True

instance BuildDiffTree Reference where
  buildDiffTree ref = Leaf (ref ^. refUuid) True

getAllDiffTreeChildren :: DiffTree -> [DiffTree]
getAllDiffTreeChildren (Leaf uuid value) = []
getAllDiffTreeChildren (Node uuid value children) = Leaf uuid value : concat (getAllDiffTreeChildren <$> children)

convertToErrorState :: MigrationState -> MigrationError -> MigrationState
convertToErrorState state error =
  let errorState = state & msStatus .~ MSError
  in errorState & msError .~ error

