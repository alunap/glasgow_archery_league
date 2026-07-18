import { useEffect, useState } from 'react'
import { Link } from 'react-router-dom'
import api from '../api.js'

export default function Home() {
  const [health, setHealth] = useState(null)
  const [error, setError] = useState('')
  const [msg, setMsg] = useState('')

  const load = async () => {
    try { setHealth(await api.health()); setError('') }
    catch (e) { setError(e.message) }
  }
  useEffect(() => { load() }, [])

  const sync = async () => {
    try { setMsg('Syncing DuckDB…'); const r = await api.sync(); setMsg(`DuckDB synced — event_scores: ${r.event_scores}`) }
    catch (e) { setMsg(''); setError(e.message) }
  }

  const links = [
    ['Import monthly Excel', '/import', 'Upload the Team Scores xlsx, review new archers, commit to Postgres + DuckDB'],
    ['Archers', '/archers', 'View and edit archer details (name, club, bowstyle, sex)'],
    ['Venues', '/venues', 'View and edit tournament venues and locations'],
    ['Events', '/events', 'View and edit event dates, venues, and stage numbers'],
    ['Scores', '/scores', 'View and edit individual event_scores; filter by event/bowstyle'],
    ['Badges', '/badges', 'View and edit badge thresholds per bowstyle (Postgres only)']
  ]

  return (
    <div>
      <h1>Glasgow League Admin</h1>
      <p className="muted">
        Admin tool for the Glasgow Archery League. Writes go to the Postgres master and
        automatically refresh <code>data/data.db</code> (DuckDB) so the Quarto site stays in sync.
      </p>

      {error && <div className="error">DB error: {error}</div>}
      {msg && <div className="success">{msg}</div>}

      <div className="card">
        <div className="row between">
          <h2 style={{ margin: 0 }}>Database</h2>
          <button onClick={sync}>Refresh DuckDB now</button>
        </div>
        {health ? (
          <table>
            <thead><tr><th>Table</th><th>Rows</th></tr></thead>
            <tbody>
              {Object.entries(health.tables || {}).map(([t, n]) => (
                <tr key={t}><td>{t}</td><td>{n}</td></tr>
              ))}
            </tbody>
          </table>
        ) : <p className="muted">Loading…</p>}
      </div>

      <h2>Pages</h2>
      {links.map(([label, to, desc]) => (
        <div key={to} className="card">
          <Link to={to} style={{ textDecoration: 'none', color: 'var(--accent)', fontWeight: 600, fontSize: '1rem' }}>{label}</Link>
          <div className="muted" style={{ fontSize: '0.85rem', marginTop: '0.25rem' }}>{desc}</div>
        </div>
      ))}
    </div>
  )
}
