# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Data Stewardship Wizard (DSW) Engine Backend — a Haskell multi-package backend with two applications:
- **Wizard Server** — core DSW app (knowledge models, projects/questionnaires, document generation, user/tenant management, real-time WebSocket collaboration)
- **Registry Server** — package registry for knowledge models and organizations

Both share code via `shared-common`. Built with Servant (type-safe REST APIs + Swagger), PostgreSQL, MinIO/S3, and background workers.

## Build & Run

Stack-based project (GHC 9.10.1, resolver nightly-2024-01-10).

```bash
# Build
stack build wizard-server
stack build registry-server
stack build shared-common

# Run (Wizard requires engine-jinja C/Python library in ./lib)
export PYTHONPATH=$(pwd)/lib:$PYTHONPATH
export DYLD_LIBRARY_PATH=$(pwd)/lib:$DYLD_LIBRARY_PATH  # macOS
stack exec wizard-server
```

Config files live in `{wizard,registry}-server/config/` (YAML). See `application.yml.example` for structure.

## Testing

Tests use HSpec. Both unit and integration tests exist (integration tests hit a real database).

```bash
stack test wizard-server
stack test registry-server

# With coverage
stack test wizard-server --jobs=1 --fast --coverage --ghc-options "-fforce-recomp"
```

Test source: `{wizard,registry}-server/test/`. Tests run DB migrations via `*.Database.Migration.Development` before the suite.

## Code Style

**Formatter:** Fourmolu (0.8.2.0) — config in `fourmolu.yaml` (2-space indent, leading arrows, indent wheres)
```bash
fourmolu -i $(find wizard-server/src -name '*.hs')
```

**Linter:** HLint (3.5) — config in `.hlint.yaml`
```bash
hlint wizard-server
hlint registry-server
hlint shared-common
```

**Spell check:** CSpell — config in `.cspell/cspell.json`

CI enforces all three. GHC warnings are treated as errors in CI.

## Package Structure

```
shared-common/       — shared library (utilities, KM domain models, DB pooling, workers, localization)
wizard-server/       — Wizard application
wizard-public/       — Wizard public-facing types
registry-server/     — Registry application
registry-public/     — Registry public-facing types
lib/                 — engine-jinja C/Python library for document templating
```

## Layered Architecture (per application)

Each app follows the same layer pattern under `src/`:

- **Api/Handler/** — REST endpoint handlers (Servant). One module per resource (User, Document, Project, KnowledgeModel, etc.)
- **Api/Resource/** — Swagger/API spec definitions
- **Service/** — Business logic (KnowledgeModel, Project, Document, User, Tenant, etc.)
- **Database/DAO/** — Data access objects (SQL queries per entity)
- **Database/Migration/Production/** — Versioned PostgreSQL migrations (numbered v1–v69+)
- **Database/Migration/Development/** — Schema creation for test environments
- **Database/Mapping/** — SQL ↔ Haskell type mappings
- **Model/** — Domain types and DTOs; Context types (BaseContext for shared config, AppContext per-request)
- **Worker/** — Background cron and permanent workers
- **Integration/Http/** — External HTTP clients (Registry, etc.)
- **Cache/** — In-memory caching (MemCache)
- **S3/** — Object storage client

## Naming Conventions

- **Handler** — API handler module
- **DTO** — request/response structures
- **Service** — business logic
- **Mapper** — data transformations
- **DAO** — database operations
- **Migration** — database migration functions

## Git Workflow

- `main` — releases only (protected)
- `develop` — development base
- `feature/*`, `chore/*`, `fix/*` — branch from develop, rebase-merge back
- `release/*`, `hotfix/*` — release management
- Jira references: `[DSW-XXX]` in branch names/commits
