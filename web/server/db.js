import pg from 'pg'
import dotenv from 'dotenv'
import { fileURLToPath } from 'url'
import path from 'path'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
// load .env from web/ root (one level up from server/).
// override: true so the file is authoritative even if the shell has a stale
// PGPASSWORD='' (e.g. left over from an earlier psql invocation) that would
// otherwise shadow the real value and break SASL auth.
dotenv.config({ path: path.resolve(__dirname, '..', '.env'), override: true })

const config = {
  host: process.env.PGHOST || 'localhost',
  port: Number(process.env.PGPORT || 5432),
  user: process.env.PGUSER || 'postgres',
  password: process.env.PGPASSWORD || '',
  database: process.env.PGDATABASE || 'glasgow_archery_league'
}

export const pool = new pg.Pool(config)

export const DUCKDB_PATH = path.resolve(
  __dirname,
  '..',
  process.env.DUCKDB_PATH || '../data/data.db'
)

export const PORT = Number(process.env.PORT || 5050)

// Quick connectivity probe for the health endpoint
export async function dbHealth() {
  if (!process.env.PGHOST) {
    throw new Error('PGHOST not set — fill in web/.env (see .env.example)')
  }
  if (!process.env.PGPASSWORD) {
    throw new Error('PGPASSWORD not set — fill in web/.env')
  }
  const client = await pool.connect()
  try {
    const { rows } = await client.query(`
      SELECT table_name, (SELECT count(*) FROM information_schema.columns c
        WHERE c.table_schema='public' AND c.table_name=t.table_name) AS cols
      FROM information_schema.tables t
      WHERE t.table_schema='public' AND t.table_type='BASE TABLE'
      ORDER BY table_name
    `)
    const counts = {}
    for (const r of rows) {
      const res = await client.query(`SELECT count(*) AS n FROM ${r.table_name}`)
      counts[r.table_name] = Number(res.rows[0].n)
    }
    return { ok: true, tables: counts }
  } finally {
    client.release()
  }
}
