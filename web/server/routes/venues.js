import { Router } from 'express'
import { pool } from '../db.js'
import { syncDuckDB } from '../duckdb-sync.js'

const router = Router()

router.get('/', async (_req, res) => {
  const { rows } = await pool.query('SELECT * FROM venues ORDER BY id')
  res.json(rows)
})

router.get('/:id', async (req, res) => {
  const { rows } = await pool.query('SELECT * FROM venues WHERE id = $1', [req.params.id])
  if (!rows.length) return res.status(404).json({ error: 'not found' })
  res.json(rows[0])
})

router.post('/', async (req, res) => {
  const { location, town, postcode, w3w, lat, lon } = req.body
  if (!location) return res.status(400).json({ error: 'location required' })
  const { rows } = await pool.query(
    `INSERT INTO venues(location, town, postcode, w3w, lat, lon) VALUES ($1,$2,$3,$4,$5,$6) RETURNING *`,
    [location, town || null, postcode || null, w3w || null, lat || null, lon || null]
  )
  await syncDuckDB()
  res.status(201).json(rows[0])
})

router.put('/:id', async (req, res) => {
  const { location, town, postcode, w3w, lat, lon } = req.body
  const { rows } = await pool.query(
    `UPDATE venues SET location=COALESCE($2,location), town=COALESCE($3,town), postcode=COALESCE($4,postcode), w3w=COALESCE($5,w3w), lat=COALESCE($6,lat), lon=COALESCE($7,lon) WHERE id=$1 RETURNING *`,
    [req.params.id, location || null, town || null, postcode || null, w3w || null, lat || null, lon || null]
  )
  if (!rows.length) return res.status(404).json({ error: 'not found' })
  await syncDuckDB()
  res.json(rows[0])
})

router.delete('/:id', async (req, res) => {
  const { rowCount } = await pool.query('DELETE FROM venues WHERE id=$1', [req.params.id])
  if (!rowCount) return res.status(404).json({ error: 'not found' })
  await syncDuckDB()
  res.json({ deleted: rowCount })
})

export default router
