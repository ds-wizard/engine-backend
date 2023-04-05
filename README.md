# Data Stewardship Wizard Engine Backend

[![User Guide](https://img.shields.io/badge/docs-User%20Guide-informational)](https://guide.ds-wizard.org)
[![Engine Backend CI](https://github.com/ds-wizard/engine-backend/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/ds-wizard/engine-backend/actions/workflows/build.yml)
[![License](https://img.shields.io/github/license/ds-wizard/engine-backend)](LICENSE)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4975/badge)](https://bestpractices.coreinfrastructure.org/projects/4975)

*Backend application for Data Stewardship Wizard*

## Applications

- Wizard (`<application>` = engine-wizard)
- Registry (`<application>` = engine-registry)

## Contribute

For contributing guidelines, please read [CONTRIBUTING](CONTRIBUTING.md) 
and [relevant section in our guide](https://img.shields.io/github/license/ds-wizard/document-worker).

### Requirements

 - **Stack** (recommended 2.9.1 or higher)
 - **Postgres & libpq** (recommended 11)
 - **Fourmolu** (recommended 0.8.2.0, optional)
 - **HLint** (recommended 3.4.1, optional)
 - **Docker** (recommended 19.03.0-ce) - *for build of production image*
 - [**document-worker**](https://github.com/ds-wizard/engine-tools) (corresponding version)
 - [**mailer**](https://github.com/ds-wizard/engine-tools) (corresponding version)

### Build & Run

For running application it's need to run MongoDB database and set up connection in configuration file.

Run these comands from the root of the project

```bash
$ stack build <application>
$ stack exec <application>
```

### Run tests

Run these comands from the root of the project

```bash
$ stack test <application>
```

### Format code

Create a bash script which will do the work for you. Run the script from the root of the project

```bash
$ fourmolu -i (find <application>/src -name '*.hs')
$ fourmolu -i (find <application>/test -name '*.hs')
```

### Code coverage

Run these comands from the root of the project

```bash
$ stack build <application>
$ stack test <application> --jobs=1 --fast --coverage --ghc-options "-fforce-recomp"
```

### Build an app version and built date

Run these comands from the `scripts` folder

```bash
$ ./<application>/build-info.sh
```

## License

This project is licensed under the Apache License v2.0 - see the [LICENSE](LICENSE.md) file for more details.
