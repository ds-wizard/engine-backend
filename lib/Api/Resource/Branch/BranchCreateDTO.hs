module Api.Resource.Branch.BranchCreateDTO where

data BranchCreateDTO = BranchCreateDTO
  { _branchCreateDTOName :: String
  , _branchCreateDTOKmId :: String
  , _branchCreateDTOParentPackageId :: Maybe String
  }
