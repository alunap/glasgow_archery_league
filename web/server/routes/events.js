import { Router } from 'express'
import { pool } from '../db.js'
import { syncDuckDB } from '../duckdb-sync.js'

const router = Router()

// GET /api/events  — includes venue location via join
router.get('/', async (_req, res) => {
  const { rows } = await pool.query(`
    SELECT e.id, e.date_of_event, e.venue_id, e.round, v.location AS venue_name, v.town
    FROM events e LEFT JOIN venues v ON e.venue_id = v.id
    ORDER BY e.date_of_event DESC
  `)
  res.json(rows)
})

router.get('/:id', async (req, res) => {
  const { rows } = await pool.query(
    `SELECT e.*, v.location AS venue_name FROM events e LEFT JOIN venues v ON e.venue_id=v.id WHERE e.id=$1`,
    [req.params.id]
  )
  if (!rows.length) return res.status(404).json({ error: 'not found' })
  res.json(rows[0])
})

router.post('/', async (req, res) => {
  const { date_of_event, venue_id, round } = req.body
  if (!date_of_event || !venue_id) return res.status(400).json({ error: 'date_of_event and venue_id required' })
  const { rows } = await pool.query(
    `INSERT INTO events(date_of_event, venue_id, round) VALUES ($1,$2,$3) RETURNING *`,
    [date_of_event, venue_id, round || null]
  )
  await syncDuckDB()
  res.status(201).json(rows[0])
})

router.put('/:id', async (req, res) => {
  const { date_of_event, venue_id, round } = req.body
  const { rows } = await pool.query(
    `UPDATE events SET date_of_event=COALESCE($2,date_of_event), venue_id=COALESCE($3,venue_id), round=COALESCE($4,round) WHERE id=$1 RETURNING *`,
    [req.params.id, date_of_event || null, venue_id || null, round ?? null]
  )
  if (!rows.length) return res.status(404).json({ error: 'not found' })
  await syncDuckDB()
  res.json(rows[0])
})

router.delete('/:id', async (req, res) => {
  const { rowCount } = await pool.query('DELETE FROM events WHERE id=$1', [req.params.id])
  if (!rowCount) return res.status(404).json({ error: 'not found' })
  await syncDuckDB()
  res.json({ deleted: rowCount })
})

export default router
