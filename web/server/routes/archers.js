import { Router } from 'express'
import { pool } from '../db.js'
import { syncDuckDB } from '../duckdb-sync.js'

const router = Router()

const COLS = ['archer', 'club', 'bowstyle', 'sex']
const SEXES = ['Gents', 'Ladies']
const BOWSTYLES = ['Recurve', 'Compound', 'Barebow', 'Traditional', 'Longbow']

// GET /api/archers?club=&bowstyle=&sex=
// Includes computed `highest_badge`: the highest-threshold badge whose minimum
// is <= the archer's best event score, matched on bowstyle. Not stored in DB.
router.get('/', async (req, res) => {
  const { club, bowstyle, sex } = req.query
  const where = []
  const params = []
  if (club) { params.push(club); where.push(`a.club = $${params.length}`) }
  if (bowstyle) { params.push(bowstyle); where.push(`a.bowstyle = $${params.length}`) }
  if (sex) { params.push(sex); where.push(`a.sex = $${params.length}`) }
  const sql = `
    SELECT a.*, COALESCE(b.badge, '') AS highest_badge
    FROM archers a
    LEFT JOIN LATERAL (
      SELECT b.badge FROM badges b
      WHERE b.bowstyle = a.bowstyle
        AND b.minimum <= (SELECT MAX(s.score) FROM event_scores s WHERE s.archer_id = a.id)
      ORDER BY b.minimum DESC
      LIMIT 1
    ) b ON true
    ${where.length ? 'WHERE ' + where.join(' AND ') : ''}
    ORDER BY a.archer
  `
  const { rows } = await pool.query(sql, params)
  res.json(rows)
})

router.get('/:id', async (req, res) => {
  const { rows } = await pool.query('SELECT * FROM archers WHERE id = $1', [req.params.id])
  if (!rows.length) return res.status(404).json({ error: 'not found' })
  res.json(rows[0])
})

router.post('/', async (req, res) => {
  const { archer, club, bowstyle, sex } = req.body
  if (!archer || !club || !bowstyle) return res.status(400).json({ error: 'archer, club, bowstyle required' })
  if (sex && !SEXES.includes(sex)) return res.status(400).json({ error: `sex must be ${SEXES.join(' or ')}` })
  if (!BOWSTYLES.includes(bowstyle)) return res.status(400).json({ error: 'unknown bowstyle' })
  const { rows } = await pool.query(
    `INSERT INTO archers(archer, club, bowstyle, sex) VALUES ($1,$2,$3,$4) RETURNING *`,
    [archer, club, bowstyle, sex || null]
  )
  await syncDuckDB()
  res.status(201).json(rows[0])
})

router.put('/:id', async (req, res) => {
  const id = req.params.id
  const { archer, club, bowstyle, sex } = req.body
  if (sex && !SEXES.includes(sex)) return res.status(400).json({ error: `sex must be ${SEXES.join(' or ')}` })
  if (bowstyle && !BOWSTYLES.includes(bowstyle)) return res.status(400).json({ error: 'unknown bowstyle' })
  const { rows } = await pool.query(
    `UPDATE archers SET archer=COALESCE($2,archer), club=COALESCE($3,club), bowstyle=COALESCE($4,bowstyle), sex=COALESCE($5,sex) WHERE id=$1 RETURNING *`,
    [id, archer || null, club || null, bowstyle || null, sex || null]
  )
  if (!rows.length) return res.status(404).json({ error: 'not found' })
  await syncDuckDB()
  res.json(rows[0])
})

router.delete('/:id', async (req, res) => {
  const { rowCount } = await pool.query('DELETE FROM archers WHERE id=$1', [req.params.id])
  if (!rowCount) return res.status(404).json({ error: 'not found' })
  await syncDuckDB()
  res.json({ deleted: rowCount })
})

export const ARCHER_META = { COLS, SEXES, BOWSTYLES }
export default router
