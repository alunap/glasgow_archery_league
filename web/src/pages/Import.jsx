// Preview and review Results workbook rows before replacing an event's scores.
import { useEffect, useState, useRef } from 'react'
import api from '../api.js'
import { matchEditedArcher, prepareImportRow, selectDefaultEvent } from '../import-utils.js'

const SEXES = ['Open', 'Ladies']
const BOWSTYLES = ['Recurve', 'Compound', 'Barebow', 'Traditional']
const CLUBS = ['Glasgow', 'Strathclyde', 'East Kilbride', 'Monklands', 'Linwood', "Orion's", 'UWS', 'Giffnock']

/** Let administrators review and commit one event's workbook scores. */
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
      setEventId(selectDefaultEvent(es))
    }).catch((e) => setError(e.message))
  }, [])

  /** Parse the selected workbook and load the review rows. */
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

  /** Update a field on one new-archer review row. */
  const setNewArcherField = (i, key, val) => {
    setNewArchers((arr) => arr.map((a, idx) => (idx === i ? { ...a, [key]: val } : a)))
  }

  const catalog = preview?.archerCatalog || []
  const matchedCount = newArchers.filter((a) => matchEditedArcher(a, catalog)).length
  const missingSex = newArchers.filter((a) => !matchEditedArcher(a, catalog) && !SEXES.includes(a.sex))
  const invalidDetails = newArchers.filter((a) => !a.archer.trim() || !CLUBS.includes(a.club) || !BOWSTYLES.includes(a.bowstyle))
  const canCommit = preview && missingSex.length === 0 && invalidDetails.length === 0

  /** Submit reviewed scores for the selected event. */
  const doCommit = async () => {
    setError(''); setMsg('')
    if (missingSex.length) {
      setError(`Set sex for ${missingSex.length} new archer(s) before committing.`)
      return
    }
    if (invalidDetails.length) {
      setError(`Set a valid name, club and bowstyle for ${invalidDetails.length} archer(s) before committing.`)
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
        ...newArchers.map((n) => prepareImportRow(n, catalog))
      ]
      const r = await api.importCommit({ event_id: Number(eventId), rows })
      setMsg(`Committed: ${r.newArchers} new archer(s), ${r.scoresInserted} score(s) for event ${r.event_id}. DuckDB event_scores now ${r.duckdb?.event_scores}.`)
      const updatedEvents = events.map((ev) => Number(ev.id) === Number(eventId) ? { ...ev, has_scores: true } : ev)
      setEvents(updatedEvents)
      setEventId(selectDefaultEvent(updatedEvents))
      setPreview(null); setNewArchers([]); setFile(null)
      if (fileRef.current) fileRef.current.value = ''
    } catch (e) { setError(e.message) }
    setBusy(false)
  }

  return (
    <div>
      <h1>Import monthly Excel</h1>
      <p className="muted">
        Upload an xlsx with a "Results" sheet. The parser reads archer details and scores,
        maps Longbow to Traditional and Tens to golds. Review new archers and any sex mismatches.
        Committing replaces that event's scores in Postgres and refreshes the DuckDB mirror.
      </p>

      {error && <div className="error">{error}</div>}
      {msg && <div className="success">{msg}</div>}

      <div className="card">
        <div className="toolbar">
          <label className="muted">Event
            <select value={eventId} disabled={busy} onChange={(e) => { setEventId(e.target.value); setPreview(null); setNewArchers([]) }}>
              <option value="">Select…</option>
              {events.map((ev) => (
                <option key={ev.id} value={ev.id}>
                  {ev.id}: {(ev.date_of_event || '').slice(0, 10)} {ev.venue_name ? `@ ${ev.venue_name}` : ''} (Stage {ev.round}){ev.has_scores ? ' — imported' : ''}
                </option>
              ))}
            </select>
          </label>
          <label className="muted">xlsx file
            <input
              ref={fileRef}
              type="file"
              accept=".xlsx,.ods"
              disabled={busy}
              onChange={(e) => { setFile(e.target.files[0]); setPreview(null); setNewArchers([]) }}
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
                {preview.counts.total} rows — {preview.counts.known + matchedCount} known, {newArchers.length - matchedCount} new
              </span>
            </div>
            <p className="muted" style={{ margin: '0.5rem 0' }}>
              Event: {preview.event.id} — {(preview.event.date_of_event || '').slice(0, 10)} @ {preview.event.venue_name}
            </p>
            <p className="muted">Sheet: {preview.meta.sheetName}</p>
          </div>

          {newArchers.length > 0 && (
            <div className="card">
              <h2>Review new or corrected archers</h2>
              <datalist id="known-archer-names">
                {[...new Set(catalog.map((a) => a.archer))].map((name) => <option key={name} value={name} />)}
              </datalist>
              <table className="editable">
                <thead>
                  <tr>
                    <th>Archer</th><th>Club</th><th>Bowstyle</th><th>Sex</th>
                    <th>Score</th><th>Hits</th><th>Golds</th>
                  </tr>
                </thead>
                <tbody>
                  {newArchers.map((a, i) => {
                    const match = matchEditedArcher(a, catalog)
                    return <tr key={i} className="row-new">
                      <td className="cell-edit">
                        <input
                          aria-label="Archer name"
                          list="known-archer-names"
                          value={a.archer}
                          disabled={busy}
                          onChange={(e) => setNewArcherField(i, 'archer', e.target.value)}
                        />
                      </td>
                      <td className="cell-edit">
                        <select value={CLUBS.includes(a.club) ? a.club : ''} disabled={busy} onChange={(e) => setNewArcherField(i, 'club', e.target.value)}>
                          <option value="">—</option>
                          {CLUBS.map((c) => <option key={c} value={c}>{c}</option>)}
                        </select>
                        {!CLUBS.includes(a.club) && <span className="badge warn">unknown club: {a.club || 'blank'}</span>}
                      </td>
                      <td className="cell-edit">
                        <select value={BOWSTYLES.includes(a.bowstyle) ? a.bowstyle : ''} disabled={busy} onChange={(e) => setNewArcherField(i, 'bowstyle', e.target.value)}>
                          <option value="">—</option>
                          {BOWSTYLES.map((c) => <option key={c} value={c}>{c}</option>)}
                        </select>
                        {!BOWSTYLES.includes(a.bowstyle) && <span className="badge warn">unknown bowstyle: {a.bowstyle || 'blank'}</span>}
                      </td>
                      <td className="cell-edit">
                        <select
                          value={a.sex}
                          disabled={busy || !!match}
                          onChange={(e) => setNewArcherField(i, 'sex', e.target.value)}
                          style={!a.sex ? { borderColor: 'var(--warn)' } : undefined}
                        >
                          <option value="">—</option>
                          {SEXES.map((c) => <option key={c} value={c}>{c}</option>)}
                        </select>
                        {a.sexUnknown && !a.sex && <span className="badge warn">unknown spreadsheet sex</span>}
                        {match && <span className="badge ok">matched existing archer</span>}
                        {match && a.sex && a.sex !== match.sex && <span className="badge warn">database sex: {match.sex || 'unset'} (unchanged)</span>}
                        {match?.alreadyImported && <span className="badge warn">already imported (will be replaced)</span>}
                      </td>
                      <td>{a.score}</td><td>{a.hits}</td><td>{a.golds}</td>
                    </tr>
                  })}
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
                      {k.sexMismatch && <span className="badge warn">spreadsheet sex: {k.sheetSex}; database unchanged</span>}
                      {k.sexUnknown && <span className="badge warn">unknown spreadsheet sex</span>}
                      {!k.alreadyImported && !k.bowstyleUnknown && !k.sexMismatch && !k.sexUnknown && <span className="badge ok">ok</span>}
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
            {invalidDetails.length > 0 && (
              <span className="badge warn">{invalidDetails.length} archer(s) need a valid name, club or bowstyle</span>
            )}
          </div>
        </>
      )}
    </div>
  )
}
