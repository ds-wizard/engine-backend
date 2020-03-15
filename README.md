# Engine Backend
> It's a backend part of the engine.

[![Engine Backend CI](https://github.com/ds-wizard/engine-backend/workflows/Engine%20Backend%20CI/badge.svg?branch=master)](https://github.com/ds-wizard/engine-backend/actions)
[![License](https://img.shields.io/badge/license-Apache%202-blue.svg)](LICENSE.md)

## applications

- Wizard (<application>: engine-wizard)
- Registry (<application>: engine-registry)

## Contribute

### Requirements

 - **Stack** (recommended 2.1.3 or higher)
 - **MongoDB** (recommended 4.2.3 or higher)
 - **RabbitMQ** (recommended 3.7.8 or higher, optional)
 - **Hindent** (recommended 5.3.1 or higher, optional)
 - **HLint** (recommended 2.1.11 or higher, optional)
 - **wkhtmltopdf** (recommended 0.12.5 or higher) - *for exports in PDF format only*
 - **Pandoc** (recommended 2.2.1 or higher) - *for exports in non HTML/PDF formats only*
 - **Docker** (recommended 17.09.0-ce or higher) - *for build of production image*

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
$ find <application>/src -name '*.hs' | while read line ; do hindent $line ; done
$ find <application>/test -name '*.hs' | while read line ; do hindent $line ; done
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

### Naming conventions
- **Handler** - a module containing handler functions
- **DTO** - a module containing structures which represents request/response in API
- **Middleware** - a module containing middleware functions
- **Service** - a module containing service functions
- **Mapper** - a module containing mapper functions
- **DAO** - a module containing functions for a manipulation with data in database
- **Migration** - a module containing functions for running initial database migrations

## License
This project is licensed under the Apache License v2.0 - see the [LICENSE](LICENSE.md) file for more details.
