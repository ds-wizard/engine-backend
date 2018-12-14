module Api.Resource.Branch.BranchChangeDTO where

data BranchChangeDTO = BranchChangeDTO
  { _branchChangeDTOName :: String
  , _branchChangeDTOKmId :: String
  , _branchChangeDTOParentPackageId :: Maybe String
  }
