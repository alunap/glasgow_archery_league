// Overwrite the DuckDB mirror (data/data.db) from the Postgres master.
// Mirrors the 4 tables that copyToDuck.R mirrors and the Quarto site reads:
// venues, archers, events, event_scores. badges stays Postgres-only.
//
// Uses @duckdb/node-api (prebuilt binary) — no native build, no SQL escaping.
import { DuckDBInstance } from '@duckdb/node-api'
import { pool, DUCKDB_PATH } from './db.js'

const TABLES = [
  { name: 'venues', cols: ['id', 'location', 'town', 'postcode', 'w3w', 'lat', 'lon'],
    create: 'CREATE TABLE venues(id INTEGER, location VARCHAR, town VARCHAR, postcode VARCHAR, w3w VARCHAR, lat VARCHAR, lon VARCHAR)' },
  { name: 'archers', cols: ['id', 'archer', 'club', 'bowstyle', 'sex'],
    create: 'CREATE TABLE archers(id INTEGER, archer VARCHAR, club VARCHAR, bowstyle VARCHAR, sex VARCHAR)' },
  { name: 'events', cols: ['id', 'date_of_event', 'venue_id', 'round'],
    create: 'CREATE TABLE events(id INTEGER, date_of_event DATE, venue_id INTEGER, round INTEGER)' },
  { name: 'event_scores', cols: ['archer_id', 'score', 'hits', 'golds', 'event_id'],
    create: 'CREATE TABLE event_scores(archer_id INTEGER, score INTEGER, hits INTEGER, golds INTEGER, event_id INTEGER)' }
]

// Convert a Postgres row value into something DuckDB accepts via parameters.
function coerce(v) {
  if (v instanceof Date) return v.toISOString().slice(0, 10) // DATE as 'YYYY-MM-DD'
  return v
}

export async function syncDuckDB() {
  const client = await pool.connect()
  const inst = await DuckDBInstance.create(DUCKDB_PATH)
  const conn = await inst.connect()
  try {
    for (const t of TABLES) {
      const { rows } = await client.query(`SELECT ${t.cols.join(', ')} FROM ${t.name} ORDER BY 1`)
      await conn.run(`DROP TABLE IF EXISTS ${t.name}`)
      await conn.run(t.create)
      if (rows.length === 0) continue
      const placeholders = t.cols.map((_, i) => `$${i + 1}`).join(', ')
      const insertSql = `INSERT INTO ${t.name}(${t.cols.join(', ')}) VALUES (${placeholders})`
      for (const row of rows) {
        await conn.run(insertSql, t.cols.map((c) => coerce(row[c])))
      }
    }
    const r = await conn.run('SELECT count(*) AS n FROM event_scores')
    const out = await r.getRows()
    return { ok: true, event_scores: Number(out[0][0]) }
  } finally {
    // Close both connection AND instance so the file lock on data.db is released.
    // Otherwise the Quarto site / duckdb CLI cannot read the mirror while the server runs.
    try { conn.closeSync() } catch {}
    try { inst.closeSync() } catch { try { inst.close() } catch {} }
    client.release()
  }
}
