import { useEffect, useState } from 'react'
import api, { crud } from '../api.js'
import TableEditor from '../components/TableEditor.jsx'

const CLUBS = ['Glasgow', 'Strathclyde', 'East Kilbride', 'Monklands', 'Linwood', "Orion's", 'UWS', 'Giffnock']
const BOWSTYLES = ['Recurve', 'Compound', 'Barebow', 'Traditional', 'Longbow']
const SEXES = ['Gents', 'Ladies']

const a = crud('archers')

export default function Archers() {
  const [rows, setRows] = useState([])
  const [error, setError] = useState('')
  const [filter, setFilter] = useState({ club: '', bowstyle: '', sex: '' })

  const load = async () => {
    try {
      const q = new URLSearchParams()
      if (filter.club) q.set('club', filter.club)
      if (filter.bowstyle) q.set('bowstyle', filter.bowstyle)
      if (filter.sex) q.set('sex', filter.sex)
      setRows(await a.list(q.toString() ? '?' + q.toString() : ''))
    } catch (e) { setError(e.message) }
  }
  useEffect(() => { load() }, [filter])

  const columns = [
    { key: 'id', label: 'ID', width: '60px' },
    { key: 'archer', label: 'Archer', editable: true },
    { key: 'club', label: 'Club', editable: true, type: 'select', options: CLUBS },
    { key: 'bowstyle', label: 'Bowstyle', editable: true, type: 'select', options: BOWSTYLES },
    { key: 'sex', label: 'Sex', editable: true, type: 'select', options: SEXES },
    { key: 'highest_badge', label: 'Highest badge' }
  ]

  return (
    <div>
      <h1>Archers</h1>
      {error && <div className="error">{error}</div>}
      <div className="card">
        <div className="toolbar">
          <label className="muted">Club
            <select value={filter.club} onChange={(e) => setFilter({ ...filter, club: e.target.value })}>
              <option value="">All</option>
              {CLUBS.map((c) => <option key={c} value={c}>{c}</option>)}
            </select>
          </label>
          <label className="muted">Bowstyle
            <select value={filter.bowstyle} onChange={(e) => setFilter({ ...filter, bowstyle: e.target.value })}>
              <option value="">All</option>
              {BOWSTYLES.map((c) => <option key={c} value={c}>{c}</option>)}
            </select>
          </label>
          <label className="muted">Sex
            <select value={filter.sex} onChange={(e) => setFilter({ ...filter, sex: e.target.value })}>
              <option value="">All</option>
              {SEXES.map((c) => <option key={c} value={c}>{c}</option>)}
            </select>
          </label>
          <span className="muted">{rows.length} archers</span>
        </div>
        <TableEditor
          rows={rows}
          columns={columns}
          idKey="id"
          onAdd={async (row) => { await a.create(row); load() }}
          onSave={async (id, row) => { await a.update(id, row); load() }}
          onDelete={async (id) => { await a.remove(id); load() }}
        />
      </div>
    </div>
  )
}
