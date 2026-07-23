import { useEffect, useState } from 'react'
import { crud } from '../api.js'
import TableEditor from '../components/TableEditor.jsx'

const b = crud('badges')
const BOWSTYLES = ['Compound', 'Recurve', 'Barebow', 'Traditional', 'Longbow']
const BADGE_COLORS = ['Pink', 'Green', 'White', 'Black', 'Blue', 'Red', 'Gold', 'Purple']

export default function Badges() {
  const [rows, setRows] = useState([])
  const [error, setError] = useState('')

  const load = async () => {
    try { setRows(await b.list()) } catch (e) { setError(e.message) }
  }
  useEffect(() => { load() }, [])

  // Badges use a composite key (bowstyle + badge). The API has no /:id; we use
  // body-based PUT/DELETE, so pass idKey='' and override handlers.
  const columns = [
    { key: 'bowstyle', label: 'Bowstyle', editable: true, type: 'select', options: BOWSTYLES },
    { key: 'badge', label: 'Badge', editable: true, type: 'select', options: BADGE_COLORS },
    { key: 'minimum', label: 'Minimum score', editable: true, type: 'number' }
  ]

  // TableEditor expects an idKey; use a synthetic one.
  const withKey = rows.map((r) => ({ ...r, _key: `${r.bowstyle}|${r.badge}` }))

  return (
    <div>
      <h1>Badges</h1>
      <p className="muted">Badge thresholds per bowstyle. Stored in Postgres only (not mirrored to DuckDB).</p>
      {error && <div className="error">{error}</div>}
      <div className="card">
        <TableEditor
          rows={withKey}
          columns={columns}
          idKey="_key"
          onAdd={async (row) => { await b.create({ ...row, minimum: Number(row.minimum) }); load() }}
          onSave={async (_id, row) => { await b.update(null, { bowstyle: row.bowstyle, badge: row.badge, minimum: Number(row.minimum) }); load() }}
          onDelete={async (_id) => {
            // find the row by _key — TableEditor calls with the synthetic id; we need the actual row.
            // Workaround: prompt-free delete using last selected is not available, so re-find via the row.
            // Instead, use a confirm with the composite key text.
            const [bowstyle, badge] = _id.split('|')
            if (!confirm(`Delete ${bowstyle} / ${badge}?`)) return
            await b.remove(null, { bowstyle, badge }); load()
          }}
        />
      </div>
    </div>
  )
}
