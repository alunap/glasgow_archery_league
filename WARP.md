# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project Overview

This is a Quarto website project that tracks results and statistics for the Glasgow Archery League. The website displays archery tournament results, team standings, and individual archer scores across multiple events throughout the season.

## Key Technologies

-   **Quarto**: Website generation and document rendering
-   **R**: Data analysis and statistical computing
-   **renv**: R package dependency management
-   **DuckDB**: Local database for storing archery scores and events
-   **tidyverse**: R data manipulation and visualization

## Essential Commands

### Development Workflow

### \# Render the entire website

quarto render

# Preview the website locally (with live reload)

quarto preview

# Render specific page

quarto render index.qmd quarto render Events/events.qmd

# Render and preview a single document

quarto preview Events/east_kilbride_oct.qmd

{.bash}

### R Environment Management

``` bash
# Restore R package environment (run this first on new setup)
R -e "renv::restore()"

# Check status of R environment
R -e "renv::status()"

# Update lockfile after adding new packages
R -e "renv::snapshot()"
```

### Database Operations

**Postgres** (`glasgow_archery_league`) is the master database; **DuckDB** (`data/data.db`) is a read-only mirror that the Quarto site reads. Both hold the same four tables: `venues`, `archers`, `events`, `event_scores`. Postgres also holds a `badges` table (not mirrored).

Connection credentials are read from environment variables (`PGHOST`, `PGPORT`, `PGUSER`, `PGPASSWORD`, `PGDATABASE`) — see `.Renviron` for the R scripts and `web/.env` for the web app. Do not hardcode credentials.

The mirror is refreshed from Postgres by `functions/copyToDuck.R` (R) or automatically by the web app after every write.

## Architecture

### Directory Structure

-   `index.qmd` - Main landing page with league table
-   `_quarto.yml` - Website configuration
-   `Events/` - Tournament result pages and main events overview
-   `functions/utility_functions.R` - Core R functions for data loading and processing
-   `data/` - DuckDB database and club images
-   `renv/` - R package management
-   `_site/` - Generated website files (created by `quarto render`)

### Core R Functions (`functions/utility_functions.R`)

Key functions that power the site:

-   `load_archer_scores(theDate)` - Load archer scores for specific event date
-   `load_all_archer_scores()` - Load all archer data across all events
-   `venue(theDate)` - Get venue information for event
-   `score_table(bow, sex, scores)` - Filter and format scores by bow style and gender
-   `club_scores(club, scores)` - Get all scores for a specific club
-   `team_results(scores)` - Calculate team rankings (top 4 archers per club)

### Database Schema

The DuckDB database contains linked tables: - `events` - the date and round of each event with link to venue - `archers` - Archer details (name, club, bow style, gender) - `venues` - Tournament locations with geographical data - `event_scores` - the score for each archer at each event with links to archer and event tables.

## Web admin app (web/)

A Vite + React + Express admin app that replaces the manual R import scripts. It imports the monthly "Team Scores" Excel, provides view/edit CRUD for all five tables, and refreshes the DuckDB mirror after every write. See `web/README.md` for full setup and usage.

```bash
cd web && cp .env.example .env   # fill in PGHOST + PGPASSWORD
npm install && npm run dev        # API on :5050, app on :5173
```

Note: the monthly Excel format changed to a per-club "Team Scores" layout with bowstyle codes RC/C/BB/TRAD and a Tens column (→ golds), and no sex column. The web app handles this; the legacy `functions/import_data.R` still expects the old "Results" sheet format.

## Development Patterns

### Adding New Events

1.  Add the event row in Postgres (via the web app's Events page, or SQL) and import the monthly Excel via the web app's Import page (this also refreshes DuckDB). Legacy alternative: run `functions/import_data.R` then `functions/copyToDuck.R`.
2.  Create new event detail page in `Events/` folder (copy existing template)
3.  Update `Events/events.qmd` to include new event section
4.  Update main league table in `index.qmd` if season complete

### Event Page Structure

Each event page follows this pattern: - YAML front matter with title and date - R setup chunk loading utility functions - Team results table using `team_results()` - Individual scores by bow style using `score_table()` - Club-specific results in tabbed panels using `club_scores()`

### Data Analysis Workflow

The site automatically generates: - Team rankings based on top 4 scores per club - Individual leaderboards by bow style and gender - Progress charts and statistical visualizations - Club comparison plots showing score distributions

### Styling and Theming

-   Uses "vapor" theme specified in `_quarto.yml`
-   Custom CSS in `styles.css`
-   Club images stored in `data/` directory

## Local Development Setup

1.  Ensure R is installed with required packages
2.  Run `R -e "renv::restore()"` to install dependencies
3.  Verify Quarto is installed: `quarto --version`
4.  Preview site: `quarto preview`
5.  The site will be available at `http://localhost:XXXX` with live reload
6.  Upload site: quarto publish quarto-pub
7.  The site will be available at <https://westofscotlandarchery.quarto.pub/glasgow-archery-league/>

## Dependencies

Main R packages managed by renv: - `tidyverse` - Data manipulation and plotting - `DBI` & `duckdb` - Database connectivity\
- `here` - Path management - `knitr` & `rmarkdown` - Document rendering - `english` - Number-to-word conversion - `lubridate` - Date handling

All dependencies are locked in `renv.lock` and managed automatically.

## Notes

-   This is a website project, not an R package (no DESCRIPTION, NAMESPACE, etc.)
-   Data updates happen through database modifications rather than code changes
-   The scoring system awards points based on team placement (1st=5pts, 2nd=4pts, etc.)
-   Team scores are sum of top 4 individual scores regardless of bow style
-   The site generates both current season standings and historical analysis
