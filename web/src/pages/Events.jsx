import { useEffect, useState } from 'react'
import api, { crud } from '../api.js'
import TableEditor from '../components/TableEditor.jsx'

const e = crud('events')

export default function Events() {
  const [rows, setRows] = useState([])
  const [venues, setVenues] = useState([])
  const [error, setError] = useState('')

  const load = async () => {
    try {
      setRows(await e.list())
      setVenues(await api.venues())
    } catch (err) { setError(err.message) }
  }
  useEffect(() => { load() }, [])

  const columns = [
    { key: 'id', label: 'ID', width: '60px' },
    { key: 'date_of_event', label: 'Date', editable: true, type: 'date', width: '140px' },
    {
      key: 'venue_id', label: 'Venue', editable: true, type: 'select',
      options: venues.map((v) => v.id),
      render: (r) => r.venue_name ? `${r.venue_name} (${r.town || ''})` : r.venue_id
    },
    { key: 'round', label: 'Round/Stage', editable: true, type: 'number', width: '110px' },
    {
      key: 'venue_name', label: 'Venue name',
      render: (r) => r.venue_name || ''
    }
  ]

  return (
    <div>
      <h1>Events</h1>
      {error && <div className="error">{error}</div>}
      <div className="card">
        <TableEditor
          rows={rows}
          columns={columns}
          idKey="id"
          onAdd={async (row) => {
            const body = { date_of_event: row.date_of_event, venue_id: Number(row.venue_id), round: row.round ? Number(row.round) : null }
            await e.create(body); load()
          }}
          onSave={async (id, row) => {
            const body = {
              date_of_event: row.date_of_event || undefined,
              venue_id: row.venue_id ? Number(row.venue_id) : undefined,
              round: row.round ? Number(row.round) : undefined
            }
            await e.update(id, body); load()
          }}
          onDelete={async (id) => { await e.remove(id); load() }}
        />
      </div>
    </div>
  )
}
