module Wizard.Database.Migration.Development.Project.ProjectMigration where

import Data.Foldable (traverse_)

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.DAO.Project.ProjectCommentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectFileDAO
import Wizard.Database.DAO.Project.ProjectPermDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.S3.Project.ProjectFileS3

runMigration = do
  logInfo _CMP_MIGRATION "(Project/Project) started"
  deleteProjectFiles
  purgeBucket
  deleteProjectComments
  deleteProjectCommentThreads
  deleteProjectPerms
  deleteProjectEvents
  deleteProjects
  insertPackage germanyKmPackage
  insertProject project1
  insertProjectEvents (fEvents project1Uuid)
  traverse_ insertProjectVersion project1Versions
  insertProject project2
  insertProjectEvents (fEvents project2Uuid)
  traverse_ insertProjectVersion project2Versions
  insertProject project3
  insertProjectEvents (fEvents project3Uuid)
  traverse_ insertProjectVersion project3Versions
  insertProject differentProject
  insertProjectCommentThread cmtQ1_t1
  insertProjectComment cmtQ1_t1_1
  insertProjectComment cmtQ1_t1_2
  insertProjectCommentThread cmtQ2_t1
  insertProjectComment cmtQ2_t1_1
  logInfo _CMP_MIGRATION "(Project/Project) ended"
