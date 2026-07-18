# Glasgow League Admin (web/)

A JavaScript/React admin app for the Glasgow Archery League. It replaces the
manual R import scripts (`functions/import_data.R`, `functions/copyToDuck.R`)
with a browser UI that imports the monthly Excel results, lets you review and
edit every table, and keeps the DuckDB mirror (`data/data.db`) in sync with the
Postgres master so the Quarto site stays current.

## Architecture

```
Excel (monthly) ──▶ [web app] ──▶ Postgres (master) ──▶ DuckDB (data/data.db) ──▶ Quarto site
                      │                 ▲
                      └── Express API ──┘  (pg)
```

- **Postgres** (`glasgow_archery_league`) is the source of truth. All writes go here.
- **DuckDB** (`data/data.db`) is a read-only mirror that the Quarto site reads.
  The web app refreshes it from Postgres after every write (parity with
  `functions/copyToDuck.R`).
- **Express API** (`server/`) talks to Postgres with `pg` and to DuckDB with
  `@duckdb/node-api` (prebuilt binary — no native compile).
- **Vite + React** (`src/`) is the frontend; in dev Vite proxies `/api` to the
  Express server, in production Express serves the built `dist/`.

### Tables
| Table | Mirrored to DuckDB? | Notes |
|---|---|---|
| `archers` | yes | id, archer, club, bowstyle, sex |
| `events` | yes | id, date_of_event, venue_id, round (stage 1–6) |
| `event_scores` | yes | archer_id, event_id, score, hits, golds |
| `venues` | yes | id, location, town, postcode, w3w, lat, lon |
| `badges` | **no** (Postgres only) | bowstyle, badge, minimum |

There are no views in either database.

## Prerequisites
- Node 18+ (tested on Node 26)
- A reachable Postgres `glasgow_archery_league` with the five tables above
- The DuckDB mirror path (default `../data/data.db`)

## Setup
```bash
cd web
cp .env.example .env       # then fill in PGHOST and PGPASSWORD
npm install                # approve native install scripts if prompted (esbuild)
npm run dev
```
- API on http://localhost:5050
- App on http://localhost:5173 (proxies `/api` → 5050, so no CORS config needed)

If `npm install` blocks install scripts for `esbuild`, approve them:
`npm install-scripts approve esbuild`.

### `.env`
`web/.env` is gitignored. Fill in `PGHOST` and `PGPASSWORD` (the rest have
sensible defaults). `PGPASSWORD` may be quoted or unquoted. The server loads
`.env` with `override: true`, so the file wins over any stale shell-level
`PGPASSWORD` (e.g. an empty one left by `psql`). See `.env.example`.

## The import flow
The monthly Excel is a single sheet named **"Team Scores"**, laid out as
per-club blocks (a club header row, then archer rows with name, bowstyle code,
score, hits, tens). This differs from the old "Results" sheet format.

1. Go to **Import**, pick the event (date / venue / stage), upload the `.xlsx`.
2. **Preview** parses the sheet and splits rows into:
   - **Known archers** — matched on `(archer, club, bowstyle)` against the
     `archers` table. Rows already imported for that event are flagged.
   - **New archers** — no match. An archer who has **changed club or bowstyle**
     legitimately appears here and should be added as a new row (this is why
     the match key includes club and bowstyle, not just the name). Spelling
     variants also appear here — fix them in the sheet or merge via the Archers
     page rather than creating a duplicate.
3. For every new archer, set **Sex** (Gents/Ladies) — required. The Commit
   button is disabled until all new archers have a sex.
4. **Commit** runs in a single Postgres transaction: inserts new archers,
   replaces that event's `event_scores` (idempotent re-import), then refreshes
   the DuckDB mirror. A summary is shown (new archers / scores inserted / DuckDB
   row count).

Bowstyle codes are mapped: `RC`→Recurve, `C`→Compound, `BB`→Barebow,
`TRAD`→Traditional, `LB`→Longbow. Club `Orions`→`Orion's`. The sheet's **Tens**
column maps to `golds`. The sheet has **no sex column** — that's why sex is set
in the review step. Unknown bowstyle codes are flagged in the UI.

## Pages
- **Home** — DB health (per-table row counts) and a manual "Refresh DuckDB" button.
- **Import** — the two-step import above.
- **Archers / Venues / Events / Badges** — sortable, inline-editable CRUD tables.
- **Scores** — filter by event and bowstyle; edit score/hits/golds inline.

## API
All under `/api`:
- `GET /health`, `POST /sync`
- `GET|POST /archers`, `GET|PUT|DELETE /archers/:id` (filter via `?club=&bowstyle=&sex=`)
- `GET|POST /venues`, `GET|PUT|DELETE /venues/:id`
- `GET|POST /events`, `GET|PUT|DELETE /events/:id`
- `GET|POST|PUT|DELETE /scores` (composite key `archer_id+event_id` in body; filter via `?event_id=&club=&bowstyle=`)
- `GET|POST /badges`, `PUT|DELETE /badges` (composite key `bowstyle+badge`)
- `POST /import/preview` (multipart: `file`, `event_id`)
- `POST /import/commit` (json: `{ event_id, rows:[{archer,club,bowstyle,sex,score,hits,golds,archer_id?,isNew}] }`)

Every write endpoint refreshes the DuckDB mirror before returning.

## Production
```bash
npm run build      # vite build → dist/
npm start          # node server/index.js serves dist/ + /api on PORT
```

## Notes
- The `badges` table is intentionally not mirrored to DuckDB — the Quarto site
  doesn't read it.
- A unique index `event_scores(archer_id, event_id)` is created automatically on
  server boot to support score upserts.
- The DuckDB instance is opened and closed on each sync so the file lock is
  released — the Quarto site and the `duckdb` CLI can read `data/data.db` while
  the server runs.
