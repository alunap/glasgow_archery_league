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

## Human Comprehension Plan: importing results
The importer reads the **Results** worksheet, not the older Team Scores layout.
The current workbook labels its Archer and Club columns in reverse: the cells
actually contain target, archer, club, sex, bowstyle, score, hits, and tens, in
that order. The parser reads the cell positions and maps tens to `golds`.
Blank or zero-only score rows are skipped; missing, unsupported or empty Results
sheets and invalid numeric values are rejected.

1. Go to **Import** and upload the workbook. The default event is the most
   recent event on or before the local date without any recorded scores. If
   none qualifies, select an event manually. Events with existing scores remain
   available and are labelled as imported. Select **Preview**.
2. Review the known archers, matched on `(archer, club, bowstyle)`. An archer
   whose club or bowstyle changed appears as new and gets a separate archer
   record, preserving links from earlier results. For a misspelled name, edit
   the name in the review table or choose a suggested existing name. An exact
   match on name, club and bowstyle immediately reuses the existing archer;
   the stored name and earlier results are not changed.
3. Review the new archers. Sex is filled from the sheet when recognized; choose
   a valid sex, club and bowstyle for any missing or unknown values before
   committing. A known archer's stored sex is never changed by import; a
   different spreadsheet sex is flagged for review. If edited details match an
   existing archer, the match is indicated and any sex discrepancy is flagged.
4. **Commit** inserts new archers and replaces all scores for the selected event
   in one Postgres transaction, then refreshes the DuckDB mirror. Review the
   preview carefully: an existing event's scores will be replaced.

Sex `O` maps to `Open` and `F` maps to `Ladies`. Longbow (including `LB`) maps
to Traditional; the parser also recognizes the common Recurve, Compound,
Barebow and Traditional codes. Club aliases are mapped to database names:
`EK Archery Club` → East Kilbride, `Orion's Archers` → Orion's,
`Strathclyde AC` → Strathclyde, `Glasgow Archers` (also `Glagow Archers`) →
Glasgow, `UWS Archery Club` → UWS, `Monklands AC` → Monklands,
`Linwood AC` → Linwood and `Giffnock Archers` → Giffnock.

Run `npm test` in `web/` for synthetic workbook and mocked API tests, and
`npm run build` to check the frontend. Neither command imports live results.

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
