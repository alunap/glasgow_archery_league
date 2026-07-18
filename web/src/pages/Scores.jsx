import { useEffect, useState } from 'react'
import api, { crud } from '../api.js'
import TableEditor from '../components/TableEditor.jsx'

const s = crud('scores')
const BOWSTYLES = ['Recurve', 'Compound', 'Barebow', 'Traditional', 'Longbow']

export default function Scores() {
  const [rows, setRows] = useState([])
  const [events, setEvents] = useState([])
  const [archers, setArchers] = useState([])
  const [error, setError] = useState('')
  const [filter, setFilter] = useState({ event_id: '', bowstyle: '' })

  const load = async () => {
    try {
      setEvents(await api.events())
      setArchers(await api.archers())
      const q = new URLSearchParams()
      if (filter.event_id) q.set('event_id', filter.event_id)
      if (filter.bowstyle) q.set('bowstyle', filter.bowstyle)
      setRows(await s.list(q.toString() ? '?' + q.toString() : ''))
    } catch (e) { setError(e.message) }
  }
  useEffect(() => { load() }, [filter])

  const withKey = rows.map((r) => ({ ...r, _key: `${r.archer_id}|${r.event_id}` }))

  const columns = [
    { key: 'date_of_event', label: 'Date', width: '120px', render: (r) => (r.date_of_event || '').slice(0, 10) },
    { key: 'round', label: 'Stage', width: '70px' },
    { key: 'archer', label: 'Archer' },
    { key: 'club', label: 'Club' },
    { key: 'bowstyle', label: 'Bowstyle' },
    { key: 'sex', label: 'Sex', width: '80px' },
    { key: 'score', label: 'Score', editable: true, type: 'number', width: '80px' },
    { key: 'hits', label: 'Hits', editable: true, type: 'number', width: '80px' },
    { key: 'golds', label: 'Golds', editable: true, type: 'number', width: '80px' }
  ]

  const archerOptions = archers.map((a) => a.id)
  const eventOptions = events.map((e) => e.id)

  return (
    <div>
      <h1>Scores</h1>
      {error && <div className="error">{error}</div>}
      <div className="card">
        <div className="toolbar">
          <label className="muted">Event
            <select value={filter.event_id} onChange={(e) => setFilter({ ...filter, event_id: e.target.value })}>
              <option value="">All</option>
              {events.map((ev) => (
                <option key={ev.id} value={ev.id}>
                  {ev.id}: {(ev.date_of_event || '').slice(0, 10)} {ev.venue_name ? `@ ${ev.venue_name}` : ''} (Stage {ev.round})
                </option>
              ))}
            </select>
          </label>
          <label className="muted">Bowstyle
            <select value={filter.bowstyle} onChange={(e) => setFilter({ ...filter, bowstyle: e.target.value })}>
              <option value="">All</option>
              {BOWSTYLES.map((c) => <option key={c} value={c}>{c}</option>)}
            </select>
          </label>
          <span className="muted">{rows.length} scores</span>
        </div>
        <TableEditor
          rows={withKey}
          columns={columns}
          idKey="_key"
          addLabel="Add score"
          onAdd={async (row) => {
            // row here is the new-row buffer with synthetic fields; prompt-based selection instead
            const archerId = Number(prompt('Archer ID (see Archers page):'))
            const eventId = Number(prompt('Event ID (see Events page):'))
            if (!archerId || !eventId) return
            const score = Number(prompt('Score:') || 0)
            const hits = Number(prompt('Hits:') || 0)
            const golds = Number(prompt('Golds:') || 0)
            await s.create({ archer_id: archerId, event_id: eventId, score, hits, golds })
            load()
          }}
          onSave={async (_key, row) => {
            const [archer_id, event_id] = _key.split('|').map(Number)
            await s.update(null, {
              archer_id, event_id,
              score: Number(row.score), hits: Number(row.hits), golds: Number(row.golds)
            })
            load()
          }}
          onDelete={async (_key) => {
            const [archer_id, event_id] = _key.split('|').map(Number)
            await s.remove(null, { archer_id, event_id })
            load()
          }}
        />
      </div>
    </div>
  )
}
