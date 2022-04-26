# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other
method with the owners of this repository before making a change.

## Development and Code Style

### Code style

* Use [HLint](https://github.com/ndmitchell/hlint) for Haskell code suggestions (see CI)
* Use [Hindent](https://github.com/mihaimaruseac/hindent) for code formatting (see CI)
* (recommended) Use extensions for your IDE/editor that integrate the tools above

### Naming conventions

* **Handler** - a module containing handler functions
* **DTO** - a module containing structures which represents request/response in API
* **Middleware** - a module containing middleware functions
* **Service** - a module containing service functions
* **Mapper** - a module containing mapper functions
* **DAO** - a module containing functions for a manipulation with data in database
* **Migration** - a module containing functions for running initial database migrations

### Tests and Performance

* New or changed functionality must be covered by tests. Please read also the [test policy](https://guide.ds-wizard.org/miscellaneous/development/contributing#test-policy).
* Pay attention to performance and potential edge cases, backend has to be robust.

## Pull Request Process

1. Ensure any unnecessary install or build dependencies and other generated files are removed (adjust `.gitignore` or `.dockerignore` if necessary).
2. Explain the changes and update in the Pull Request message. If it affects our [User Guide](https://guide.ds-wizard.org), 
   state explicitly how it should be changed.
3. Be ready to communicate about the Pull Request and make changes if required by reviewers.
4. The Pull Request may be merged once it passes the review and automatic checks.

## Gitflow Workflow

We use the standard [Gitflow Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow):

* __main__ branch is used only for releases (and eventually hotfixes), this branch is also protected on GitHub (pull
  requests with review and all checks must pass)
* __develop__ branch is used for development and as a base for following development branches of features, support
  stuff, and as a base for releases
* __feature/*__ (base develop, rebase-merged back to develop when done)
* __chore/*__ (like the feature but semantically different, not the feature but some chore, e.g., cleanup or update of
  Dockerfile)
* __fix/*__ (like the feature but semantically different, not something new but fix of a non-critical bug)
* __release/*__ (base develop, merged to main and develop when ready for release+tag)
* __hotfix/*__ (base main, merged to main and develop)

Please note, that for tasks from [our Jira](https://ds-wizard.atlassian.net/projects/DSW/issues), we use such
as `[DSW-XXX]` identifying the project and task number.

## Release Management

For the release management we use (aside from the [Gitflow Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow)):

* [Semantic versioning](https://semver.org)
* Release Candidates - X.Y.Z-rc.N should be created if don’t expect any problems (in that case use alpha or beta), and
  make a walkthrough to verify its functionality according to the manuals finally - it also verifies that the
  documentation is up to date with the new version.
* Docker Hub image - in case of release, Docker image with the same tag will be created automatically.
* Compatibility in DSW - the matching major and minor version of DSW components must be compatible.

The changes must be captured in our [User Guide](https://guide.ds-wizard.org).
