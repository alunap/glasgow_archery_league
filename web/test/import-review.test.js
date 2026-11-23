// Tests for import-page rematching and default-event selection.
import assert from 'node:assert/strict'
import { once } from 'node:events'
import test from 'node:test'
import express from 'express'
import { pool } from '../server/db.js'
import eventsRoute from '../server/routes/events.js'

test('defaults to the latest unscored event on or before the local date', async () => {
  const { selectDefaultEvent } = await import('../src/import-utils.js')
  const events = [
    { id: 999, date_of_event: '2026-12-01', has_scores: false },
    { id: 100, date_of_event: '2026-10-20', has_scores: false },
    { id: 2, date_of_event: '2026-11-22', has_scores: false },
    { id: 9, date_of_event: '2026-11-21', has_scores: false },
    { id: 10, date_of_event: '2026-11-23', has_scores: true }
  ]
  assert.equal(selectDefaultEvent(events, new Date(2026, 10, 23, 0, 5)), '2')
  assert.equal(selectDefaultEvent([{ id: 3, date_of_event: '2026-11-23', has_scores: false }, ...events], new Date(2026, 10, 23, 23, 55)), '3')
  assert.equal(selectDefaultEvent(events.filter((event) => event.id !== 2), new Date(2026, 10, 23)), '9')
  assert.equal(selectDefaultEvent(events.map((event) => ({ ...event, has_scores: true })), new Date(2026, 10, 23)), '')
  assert.equal(selectDefaultEvent([], new Date(2026, 10, 23)), '')
})

test('corrected names rematch exact identities without merging club or bowstyle changes', async () => {
  const { matchEditedArcher, prepareImportRow } = await import('../src/import-utils.js')
  const catalog = [
    { archer_id: 7, archer: 'Alex Smith', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Open', alreadyImported: true },
    { archer_id: 8, archer: 'Alex Smith', club: 'Linwood', bowstyle: 'Compound', sex: 'Ladies', alreadyImported: false }
  ]
  const row = { archer: 'Alex Smth', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Ladies', score: 500, hits: 60, golds: 10 }
  assert.equal(matchEditedArcher(row, catalog), null)
  const corrected = { ...row, archer: ' Alex Smith ' }
  assert.deepEqual(matchEditedArcher(corrected, catalog), catalog[0])
  assert.deepEqual(prepareImportRow(corrected, catalog), {
    archer: 'Alex Smith', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Open',
    score: 500, hits: 60, golds: 10, archer_id: 7, isNew: false
  })
  assert.deepEqual(matchEditedArcher({ ...corrected, club: 'Linwood', bowstyle: 'Compound' }, catalog), catalog[1])
  assert.equal(matchEditedArcher({ ...corrected, club: 'Linwood' }, catalog), null)
  assert.equal(matchEditedArcher({ ...corrected, bowstyle: 'Barebow' }, catalog), null)
  assert.equal(prepareImportRow({ ...corrected, club: 'Linwood' }, catalog).isNew, true)
})

test('event listing exposes whether each event already has scores', async () => {
  const originalQuery = pool.query
  let query = ''
  pool.query = async (sql) => {
    query = sql
    return { rows: [
      { id: 1, date_of_event: '2026-11-22', has_scores: false },
      { id: 2, date_of_event: '2026-10-25', has_scores: true }
    ] }
  }
  const app = express()
  app.use('/events', eventsRoute)
  const server = app.listen(0, 'localhost')
  await once(server, 'listening')
  try {
    const response = await fetch(`http://localhost:${server.address().port}/events`)
    assert.equal(response.status, 200)
    assert.deepEqual((await response.json()).map((e) => e.has_scores), [false, true])
    assert.match(query, /EXISTS\s*\(\s*SELECT[\s\S]*FROM\s+event_scores/i)
    assert.match(query, /event_id\s*=\s*e\.id/i)
  } finally {
    pool.query = originalQuery
    await new Promise((resolve) => server.close(resolve))
  }
})
