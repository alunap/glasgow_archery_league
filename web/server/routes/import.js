import { Router } from 'express'
import multer from 'multer'
import { pool } from '../db.js'
import { syncDuckDB } from '../duckdb-sync.js'
import { parseResultsSheet } from '../excel-parser.js'

const router = Router()
const upload = multer({ storage: multer.memoryStorage() })

const SEXES = ['Gents', 'Ladies']
const key = (r) => `${r.archer}|${r.club}|${r.bowstyle}`

// Load all archers into a map key(archer|club|bowstyle) -> archer row
async function loadArcherMap() {
  const { rows } = await pool.query('SELECT id, archer, club, bowstyle, sex FROM archers')
  const map = new Map()
  for (const a of rows) map.set(key(a), a)
  return map
}

// POST /api/import/preview  (multipart: file=results.xlsx, field: event_id)
router.post('/preview', upload.single('file'), async (req, res) => {
  if (!req.file) return res.status(400).json({ error: 'xlsx file required (field "file")' })
  const eventId = Number(req.body.event_id)
  if (!eventId) return res.status(400).json({ error: 'event_id required' })

  const { meta, archers } = parseResultsSheet(req.file.buffer)
  const archerMap = await loadArcherMap()

  const { rows: eventRows } = await pool.query(
    `SELECT e.*, v.location AS venue_name FROM events e LEFT JOIN venues v ON e.venue_id=v.id WHERE e.id=$1`,
    [eventId]
  )
  if (!eventRows.length) return res.status(404).json({ error: 'event not found' })
  const event = eventRows[0]

  // Existing scores for this event, to flag rows already imported
  const { rows: existing } = await pool.query(
    'SELECT archer_id FROM event_scores WHERE event_id=$1', [eventId]
  )
  const existingIds = new Set(existing.map((r) => r.archer_id))

  const knownScores = []
  const newArchers = []
  for (const r of archers) {
    if (!r.club) { r.warning = 'no club header detected'; }
    const found = archerMap.get(key(r))
    if (found) {
      knownScores.push({
        archer_id: found.id, event_id: eventId,
        archer: r.archer, club: r.club, bowstyle: r.bowstyle, sex: found.sex,
        score: r.score, hits: r.hits, golds: r.golds,
        alreadyImported: existingIds.has(found.id),
        bowstyleUnknown: !!r.bowstyleUnknown
      })
    } else {
      newArchers.push({
        event_id: eventId,
        archer: r.archer, club: r.club, bowstyle: r.bowstyle, sex: null,
        score: r.score, hits: r.hits, golds: r.golds,
        bowstyleUnknown: !!r.bowstyleUnknown,
        isNew: true
      })
    }
  }

  res.json({
    event,
    meta,
    knownScores,
    newArchers,
    counts: { known: knownScores.length, new: newArchers.length, total: archers.length }
  })
})

// POST /api/import/commit
// body: { event_id, rows: [{ archer, club, bowstyle, sex, score, hits, golds, archer_id?, isNew }] }
router.post('/commit', async (req, res) => {
  const eventId = Number(req.body.event_id)
  const rows = Array.isArray(req.body.rows) ? req.body.rows : []
  if (!eventId) return res.status(400).json({ error: 'event_id required' })

  // Validate new archers have sex set
  const missingSex = rows.filter((r) => r.isNew && !SEXES.includes(r.sex))
  if (missingSex.length) {
    return res.status(400).json({
      error: 'sex required for new archers (Gents or Ladies)',
      missing: missingSex.map((r) => r.archer)
    })
  }

  let newArchersCount = 0
  let scoresInserted = 0
  const client = await pool.connect()
  try {
    await client.query('BEGIN')

    // Resolve archer ids: use provided archer_id for known; insert new ones.
    const archerMap = new Map()
    const all = await client.query('SELECT id, archer, club, bowstyle FROM archers')
    for (const a of all.rows) archerMap.set(key(a), a.id)

    const resolved = []
    for (const r of rows) {
      let archerId = r.archer_id
      if (!archerId) {
        const k = key(r)
        archerId = archerMap.get(k)
      }
      if (!archerId) {
        if (!r.isNew) {
          // known archer we couldn't resolve — treat as new (needs sex)
          if (!SEXES.includes(r.sex)) {
            throw new Error(`Archer "${r.archer}" not found and sex not set`)
          }
        }
        const ins = await client.query(
          'INSERT INTO archers(archer, club, bowstyle, sex) VALUES ($1,$2,$3,$4) RETURNING id',
          [r.archer, r.club, r.bowstyle, r.sex]
        )
        archerId = ins.rows[0].id
        archerMap.set(key(r), archerId)
        newArchersCount++
      }
      resolved.push({ archer_id: archerId, score: r.score, hits: r.hits, golds: r.golds })
    }

    // Replace existing scores for this event (idempotent re-import)
    await client.query('DELETE FROM event_scores WHERE event_id=$1', [eventId])

    for (const s of resolved) {
      // skip blank rows: score 0 with no hits/golds (matches R's score!=0 filter)
      if (!s.score && !s.hits && !s.golds) continue
      await client.query(
        'INSERT INTO event_scores(archer_id, event_id, score, hits, golds) VALUES ($1,$2,$3,$4,$5)',
        [s.archer_id, eventId, s.score || 0, s.hits || 0, s.golds || 0]
      )
      scoresInserted++
    }

    await client.query('COMMIT')
  } catch (err) {
    await client.query('ROLLBACK')
    client.release()
    return res.status(400).json({ error: err.message })
  }
  client.release()

  const sync = await syncDuckDB()
  res.json({ ok: true, event_id: eventId, newArchers: newArchersCount, scoresInserted, duckdb: sync })
})

export default router
