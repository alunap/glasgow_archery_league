import express from 'express'
import path from 'path'
import { fileURLToPath } from 'url'
import { pool, dbHealth, PORT } from './db.js'
import { syncDuckDB } from './duckdb-sync.js'
import archers from './routes/archers.js'
import venues from './routes/venues.js'
import events from './routes/events.js'
import scores from './routes/scores.js'
import badges from './routes/badges.js'
import importRoutes from './routes/import.js'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const app = express()
app.use(express.json({ limit: '5mb' }))

// Ensure a unique index on event_scores(archer_id, event_id) so the upsert PUT works.
async function ensureSchema() {
  try {
    await pool.query(
      'CREATE UNIQUE INDEX IF NOT EXISTS event_scores_archer_event_uniq ON event_scores(archer_id, event_id)'
    )
  } catch (e) {
    console.warn('schema bootstrap skipped:', e.message)
  }
}

app.get('/api/health', async (_req, res) => {
  try {
    const health = await dbHealth()
    res.json(health)
  } catch (e) {
    res.status(500).json({ ok: false, error: e.message })
  }
})

app.post('/api/sync', async (_req, res) => {
  try {
    const r = await syncDuckDB()
    res.json(r)
  } catch (e) {
    res.status(500).json({ error: e.message })
  }
})

app.use('/api/archers', archers)
app.use('/api/venues', venues)
app.use('/api/events', events)
app.use('/api/scores', scores)
app.use('/api/badges', badges)
app.use('/api/import', importRoutes)

// Serve the built frontend in production
const distDir = path.resolve(__dirname, '..', 'dist')
app.use(express.static(distDir))
app.get('*', (req, res, next) => {
  if (req.path.startsWith('/api')) return next()
  res.sendFile(path.join(distDir, 'index.html'), (err) => {
    if (err) next(err)
  })
})

ensureSchema().then(() => {
  app.listen(PORT, () => console.log(`glasgow-league-web API on http://localhost:${PORT}`))
})
