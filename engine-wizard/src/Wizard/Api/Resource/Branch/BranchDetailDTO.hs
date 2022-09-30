module Wizard.Api.Resource.Branch.BranchDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Branch.BranchState

data BranchDetailDTO =
  BranchDetailDTO
    { _branchDetailDTOUuid :: U.UUID
    , _branchDetailDTOName :: String
    , _branchDetailDTOKmId :: String
    , _branchDetailDTOState :: BranchState
    , _branchDetailDTOPreviousPackageId :: Maybe String
    , _branchDetailDTOForkOfPackageId :: Maybe String
    , _branchDetailDTOForkOfPackage :: Maybe PackageSimpleDTO
    , _branchDetailDTOOwnerUuid :: Maybe U.UUID
    , _branchDetailDTOEvents :: [Event]
    , _branchDetailDTOKnowledgeModel :: KnowledgeModel
    , _branchDetailDTOCreatedAt :: UTCTime
    , _branchDetailDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq BranchDetailDTO where
  a == b =
    _branchDetailDTOUuid a == _branchDetailDTOUuid b &&
    _branchDetailDTOName a == _branchDetailDTOName b &&
    _branchDetailDTOKmId a == _branchDetailDTOKmId b &&
    _branchDetailDTOState a == _branchDetailDTOState b &&
    _branchDetailDTOPreviousPackageId a == _branchDetailDTOPreviousPackageId b &&
    _branchDetailDTOForkOfPackageId a == _branchDetailDTOForkOfPackageId b &&
    _branchDetailDTOForkOfPackage a == _branchDetailDTOForkOfPackage b &&
    _branchDetailDTOOwnerUuid a == _branchDetailDTOOwnerUuid b &&
    _branchDetailDTOEvents a == _branchDetailDTOEvents b &&
    _branchDetailDTOKnowledgeModel a == _branchDetailDTOKnowledgeModel b
