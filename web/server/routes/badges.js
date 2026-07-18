import { Router } from 'express'
import { pool } from '../db.js'

const router = Router()
// badges is Postgres-only (not mirrored to DuckDB), so no sync here.

router.get('/', async (_req, res) => {
  const { rows } = await pool.query('SELECT * FROM badges ORDER BY bowstyle, minimum')
  res.json(rows)
})

router.get('/:bowstyle', async (req, res) => {
  const { rows } = await pool.query('SELECT * FROM badges WHERE bowstyle=$1 ORDER BY minimum', [req.params.bowstyle])
  res.json(rows)
})

router.post('/', async (req, res) => {
  const { bowstyle, badge, minimum } = req.body
  if (!bowstyle || !badge || minimum === undefined) return res.status(400).json({ error: 'bowstyle, badge, minimum required' })
  const { rows } = await pool.query(
    'INSERT INTO badges(bowstyle, badge, minimum) VALUES ($1,$2,$3) RETURNING *',
    [bowstyle, badge, minimum]
  )
  res.status(201).json(rows[0])
})

router.put('/', async (req, res) => {
  const { bowstyle, badge, minimum } = req.body
  if (!bowstyle || !badge) return res.status(400).json({ error: 'bowstyle and badge required' })
  const { rows } = await pool.query(
    'UPDATE badges SET minimum=COALESCE($3,minimum) WHERE bowstyle=$1 AND badge=$2 RETURNING *',
    [bowstyle, badge, minimum ?? null]
  )
  if (!rows.length) return res.status(404).json({ error: 'not found' })
  res.json(rows[0])
})

router.delete('/', async (req, res) => {
  const { bowstyle, badge } = req.body
  if (!bowstyle || !badge) return res.status(400).json({ error: 'bowstyle and badge required' })
  const { rowCount } = await pool.query('DELETE FROM badges WHERE bowstyle=$1 AND badge=$2', [bowstyle, badge])
  if (!rowCount) return res.status(404).json({ error: 'not found' })
  res.json({ deleted: rowCount })
})

export default router
