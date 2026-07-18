import { useEffect, useState } from 'react'
import { crud } from '../api.js'
import TableEditor from '../components/TableEditor.jsx'

const v = crud('venues')

export default function Venues() {
  const [rows, setRows] = useState([])
  const [error, setError] = useState('')

  const load = async () => {
    try { setRows(await v.list()) } catch (e) { setError(e.message) }
  }
  useEffect(() => { load() }, [])

  const columns = [
    { key: 'id', label: 'ID', width: '60px' },
    { key: 'location', label: 'Location', editable: true },
    { key: 'town', label: 'Town', editable: true },
    { key: 'postcode', label: 'Postcode', editable: true, width: '110px' },
    { key: 'w3w', label: 'What3words', editable: true },
    { key: 'lat', label: 'Lat', editable: true, width: '100px' },
    { key: 'lon', label: 'Lon', editable: true, width: '100px' }
  ]

  return (
    <div>
      <h1>Venues</h1>
      {error && <div className="error">{error}</div>}
      <div className="card">
        <TableEditor
          rows={rows}
          columns={columns}
          idKey="id"
          onAdd={async (row) => { await v.create(row); load() }}
          onSave={async (id, row) => { await v.update(id, row); load() }}
          onDelete={async (id) => { await v.remove(id); load() }}
        />
      </div>
    </div>
  )
}
