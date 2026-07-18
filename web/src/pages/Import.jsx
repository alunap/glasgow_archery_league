import { useEffect, useState, useRef } from 'react'
import api from '../api.js'

const SEXES = ['Gents', 'Ladies']
const BOWSTYLES = ['Recurve', 'Compound', 'Barebow', 'Traditional', 'Longbow']
const CLUBS = ['Glasgow', 'Strathclyde', 'East Kilbride', 'Monklands', 'Linwood', "Orion's", 'UWS', 'Giffnock']

export default function Import() {
  const [events, setEvents] = useState([])
  const [eventId, setEventId] = useState('')
  const [file, setFile] = useState(null)
  const [preview, setPreview] = useState(null)
  const [newArchers, setNewArchers] = useState([])
  const [error, setError] = useState('')
  const [msg, setMsg] = useState('')
  const [busy, setBusy] = useState(false)
  const fileRef = useRef(null)

  useEffect(() => {
    api.events().then((es) => {
      setEvents(es)
      // default to the latest event
      if (es.length) setEventId(String(es[0].id))
    }).catch((e) => setError(e.message))
  }, [])

  const doPreview = async () => {
    setError(''); setMsg(''); setPreview(null); setNewArchers([])
    if (!file) { setError('Choose an xlsx file first'); return }
    if (!eventId) { setError('Pick an event first'); return }
    setBusy(true)
    try {
      const fd = new FormData()
      fd.append('file', file)
      fd.append('event_id', eventId)
      const r = await api.importPreview(fd)
      setPreview(r)
      setNewArchers(r.newArchers.map((a) => ({ ...a, sex: a.sex || '' })))
    } catch (e) { setError(e.message) }
    setBusy(false)
  }

  const setNewArcherField = (i, key, val) => {
    setNewArchers((arr) => arr.map((a, idx) => (idx === i ? { ...a, [key]: val } : a)))
  }

  const missingSex = newArchers.filter((a) => !SEXES.includes(a.sex))
  const canCommit = preview && newArchers.length >= 0 && missingSex.length === 0

  const doCommit = async () => {
    setError(''); setMsg('')
    if (missingSex.length) {
      setError(`Set sex for ${missingSex.length} new archer(s) before committing.`)
      return
    }
    setBusy(true)
    try {
      const rows = [
        ...preview.knownScores.map((k) => ({
          archer: k.archer, club: k.club, bowstyle: k.bowstyle, sex: k.sex,
          score: k.score, hits: k.hits, golds: k.golds,
          archer_id: k.archer_id, isNew: false
        })),
        ...newArchers.map((n) => ({
          archer: n.archer, club: n.club, bowstyle: n.bowstyle, sex: n.sex,
          score: n.score, hits: n.hits, golds: n.golds,
          isNew: true
        }))
      ]
      const r = await api.importCommit({ event_id: Number(eventId), rows })
      setMsg(`Committed: ${r.newArchers} new archer(s), ${r.scoresInserted} score(s) for event ${r.event_id}. DuckDB event_scores now ${r.duckdb?.event_scores}.`)
      setPreview(null); setNewArchers([]); setFile(null)
      if (fileRef.current) fileRef.current.value = ''
    } catch (e) { setError(e.message) }
    setBusy(false)
  }

  return (
    <div>
      <h1>Import monthly Excel</h1>
      <p className="muted">
        Upload a "Team Scores" xlsx. The parser reads per-club blocks (bowstyle codes RC/C/BB/TRAD;
        Tens → golds). New archers are highlighted and require a sex before commit. Committing
        replaces that event's scores in Postgres and refreshes the DuckDB mirror.
      </p>

      {error && <div className="error">{error}</div>}
      {msg && <div className="success">{msg}</div>}

      <div className="card">
        <div className="toolbar">
          <label className="muted">Event
            <select value={eventId} onChange={(e) => setEventId(e.target.value)}>
              <option value="">Select…</option>
              {events.map((ev) => (
                <option key={ev.id} value={ev.id}>
                  {ev.id}: {(ev.date_of_event || '').slice(0, 10)} {ev.venue_name ? `@ ${ev.venue_name}` : ''} (Stage {ev.round})
                </option>
              ))}
            </select>
          </label>
          <label className="muted">xlsx file
            <input
              ref={fileRef}
              type="file"
              accept=".xlsx,.ods"
              onChange={(e) => setFile(e.target.files[0])}
              style={{ width: 'auto' }}
            />
          </label>
          <button className="primary" onClick={doPreview} disabled={busy}>Preview</button>
        </div>
      </div>

      {preview && (
        <>
          <div className="card">
            <div className="row between">
              <h2 style={{ margin: 0 }}>Preview</h2>
              <span className="muted">
                {preview.counts.total} rows — {preview.counts.known} known, {preview.counts.new} new
              </span>
            </div>
            <p className="muted" style={{ margin: '0.5rem 0' }}>
              Event: {preview.event.id} — {(preview.event.date_of_event || '').slice(0, 10)} @ {preview.event.venue_name}
            </p>
            {preview.meta.title && <p className="muted">Sheet: {preview.meta.sheetName} — {preview.meta.title}</p>}
          </div>

          {newArchers.length > 0 && (
            <div className="card">
              <h2>New archers — set sex (required)</h2>
              <table className="editable">
                <thead>
                  <tr>
                    <th>Archer</th><th>Club</th><th>Bowstyle</th><th>Sex</th>
                    <th>Score</th><th>Hits</th><th>Golds</th>
                  </tr>
                </thead>
                <tbody>
                  {newArchers.map((a, i) => (
                    <tr key={i} className="row-new">
                      <td>{a.archer}</td>
                      <td className="cell-edit">
                        <select value={a.club} onChange={(e) => setNewArcherField(i, 'club', e.target.value)}>
                          <option value="">—</option>
                          {CLUBS.map((c) => <option key={c} value={c}>{c}</option>)}
                        </select>
                      </td>
                      <td className="cell-edit">
                        <select value={a.bowstyle} onChange={(e) => setNewArcherField(i, 'bowstyle', e.target.value)}>
                          <option value="">—</option>
                          {BOWSTYLES.map((c) => <option key={c} value={c}>{c}</option>)}
                        </select>
                        {a.bowstyleUnknown && <span className="badge warn" style={{ marginLeft: 4 }}>unknown code</span>}
                      </td>
                      <td className="cell-edit">
                        <select
                          value={a.sex}
                          onChange={(e) => setNewArcherField(i, 'sex', e.target.value)}
                          style={!a.sex ? { borderColor: 'var(--warn)' } : undefined}
                        >
                          <option value="">—</option>
                          {SEXES.map((c) => <option key={c} value={c}>{c}</option>)}
                        </select>
                      </td>
                      <td>{a.score}</td><td>{a.hits}</td><td>{a.golds}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          <div className="card">
            <h2>Known archers (scores to import)</h2>
            <table>
              <thead>
                <tr>
                  <th>Archer</th><th>Club</th><th>Bowstyle</th><th>Sex</th>
                  <th>Score</th><th>Hits</th><th>Golds</th><th>Status</th>
                </tr>
              </thead>
              <tbody>
                {preview.knownScores.map((k, i) => (
                  <tr key={i} className={k.bowstyleUnknown ? 'row-flag' : ''}>
                    <td>{k.archer}</td><td>{k.club}</td><td>{k.bowstyle}</td><td>{k.sex}</td>
                    <td>{k.score}</td><td>{k.hits}</td><td>{k.golds}</td>
                    <td>
                      {k.alreadyImported && <span className="badge warn">already imported (will be replaced)</span>}
                      {k.bowstyleUnknown && <span className="badge danger">unknown bowstyle</span>}
                      {!k.alreadyImported && !k.bowstyleUnknown && <span className="badge ok">ok</span>}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>

          <div className="toolbar">
            <button className="primary" onClick={doCommit} disabled={busy || !canCommit}>
              Commit to Postgres + sync DuckDB
            </button>
            {missingSex.length > 0 && (
              <span className="badge warn">{missingSex.length} new archer(s) missing sex</span>
            )}
          </div>
        </>
      )}
    </div>
  )
}
