# How to contribute

## How to begin?

Go though this whole document and you will be ready.


## What to install?

- Haskell, Cabal, Stack
- Docker
- MongoDB
- wkhtmltopdf (recommended 0.12.5 or higher) - *for exports in PDF format only*

## How to setup the application for a local run?

1. Run a MongoDB database
2. Edit configuration (`config/app-config.cfg``)
3. Check

## How to build and execute application?

Run these comands from the root of the project:

1. `hpack`
2. `stack build`
3. `stack exec dsw-server`

## Test the application

Run these comands from the root of the project:

1. `hpack`
2. `stack build`
3. `stack test --jobs=1 --fast`


## How to format code?

Create a bash script which will do the work for you. Run the script from the root of the project.

```bash
find lib -name '*.hs' | while read line ; do hindent $line ; done
find test -name '*.hs' | while read line ; do hindent $line ; done
```

## How to check code coverage?

Run these comands from the root of the project:

1. `hpack`
2. `stack build`
3. `stack test --jobs=1 --fast --coverage --ghc-options "-fforce-recomp"`

## What are the naming and the code convention?

Try to keep a line with already written code. It is the best convention.

### Naming conventions for Haskell modules
- **Handler** - a module containing handler functions
- **DTO** - a module containing structures which represents request/response in API
- **Middleware** - a module containing middleware functions
- **Service** - a module containing service functions
- **Mapper** - a module containing mapper functions
- **DAO** - a module containing functions for a manipulation with data in database
- **Migration** - a module containing functions for running initial database migrations
