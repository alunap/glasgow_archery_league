import { Router } from 'express'
import { pool } from '../db.js'
import { syncDuckDB } from '../duckdb-sync.js'

const router = Router()

// GET /api/scores?event_id=&club=&bowstyle= — joined to archer + event for display
router.get('/', async (req, res) => {
  const { event_id, club, bowstyle } = req.query
  const where = []
  const params = []
  if (event_id) { params.push(event_id); where.push(`s.event_id = $${params.length}`) }
  if (club) { params.push(club); where.push(`a.club = $${params.length}`) }
  if (bowstyle) { params.push(bowstyle); where.push(`a.bowstyle = $${params.length}`) }
  const sql = `
    SELECT s.archer_id, s.event_id, s.score, s.hits, s.golds,
           a.archer, a.club, a.bowstyle, a.sex,
           e.date_of_event, e.round
    FROM event_scores s
    JOIN archers a ON s.archer_id = a.id
    JOIN events e ON s.event_id = e.id
    ${where.length ? 'WHERE ' + where.join(' AND ') : ''}
    ORDER BY e.date_of_event DESC, a.club, a.archer
  `
  const { rows } = await pool.query(sql, params)
  res.json(rows)
})

router.post('/', async (req, res) => {
  const { archer_id, event_id, score, hits, golds } = req.body
  if (!archer_id || !event_id) return res.status(400).json({ error: 'archer_id and event_id required' })
  const { rows } = await pool.query(
    `INSERT INTO event_scores(archer_id, event_id, score, hits, golds) VALUES ($1,$2,$3,$4,$5) RETURNING *`,
    [archer_id, event_id, score ?? 0, hits ?? 0, golds ?? 0]
  )
  await syncDuckDB()
  res.status(201).json(rows[0])
})

// upsert by (archer_id, event_id)
router.put('/', async (req, res) => {
  const { archer_id, event_id, score, hits, golds } = req.body
  if (!archer_id || !event_id) return res.status(400).json({ error: 'archer_id and event_id required' })
  const { rows } = await pool.query(
    `INSERT INTO event_scores(archer_id, event_id, score, hits, golds) VALUES ($1,$2,$3,$4,$5)
     ON CONFLICT (archer_id, event_id) DO UPDATE SET score=EXCLUDED.score, hits=EXCLUDED.hits, golds=EXCLUDED.golds
     RETURNING *`,
    [archer_id, event_id, score ?? 0, hits ?? 0, golds ?? 0]
  )
  await syncDuckDB()
  res.json(rows[0])
})

router.delete('/', async (req, res) => {
  const { archer_id, event_id } = req.body
  if (!archer_id || !event_id) return res.status(400).json({ error: 'archer_id and event_id required' })
  const { rowCount } = await pool.query(
    'DELETE FROM event_scores WHERE archer_id=$1 AND event_id=$2', [archer_id, event_id]
  )
  if (!rowCount) return res.status(404).json({ error: 'not found' })
  await syncDuckDB()
  res.json({ deleted: rowCount })
})

export default router
