// Regression tests for the Results workbook parser and safe import preview.
import assert from 'node:assert/strict'
import { once } from 'node:events'
import test from 'node:test'
import express from 'express'
import XLSX from 'xlsx'
import { parseResultsSheet } from '../server/excel-parser.js'
import { prepareImportRow } from '../src/import-utils.js'
import { pool } from '../server/db.js'
import importRoutes from '../server/routes/import.js'
import * as importService from '../server/routes/import.js'

const header = ['Target', 'Club', 'Archer', 'F/M', 'Bow Type', 'Score', 'Hits', '10s']

/** Create a workbook with the same offset and reversed headers as the current file. */
function workbook(rows, sheetName = 'Results') {
  const sheet = XLSX.utils.aoa_to_sheet([])
  XLSX.utils.sheet_add_aoa(sheet, [[], header, ...rows], { origin: 'B1' })
  const book = XLSX.utils.book_new()
  XLSX.utils.book_append_sheet(book, sheet, sheetName)
  return XLSX.write(book, { type: 'buffer', bookType: 'xlsx' })
}

/** Post a workbook to an isolated test server. */
async function preview(url, buffer) {
  const form = new FormData()
  form.append('event_id', '13')
  form.append('file', new Blob([buffer]), 'results.xlsx')
  return fetch(`${url}/preview`, { method: 'POST', body: form })
}

test('parses Results columns, normalizes categories and ignores empty score rows', () => {
  const buffer = workbook([
    ['1A', 'James Ross', 'EK Archery Club', 'O', 'Compound', 586, 60, 46],
    ['1B', 'Archer Two', "Orion's Archers", 'F', 'Longbow', 400, 55, 4],
    ['1C', '', '', '', '', '', '', ''],
    ['1D', 'No Score', 'Linwood AC', 'O', 'Recurve', '', '', '']
  ])
  const result = parseResultsSheet(buffer)
  assert.equal(result.meta.sheetName, 'Results')
  assert.deepEqual(result.archers, [
    { archer: 'James Ross', club: 'East Kilbride', sex: 'Open', bowstyle: 'Compound', score: 586, hits: 60, golds: 46 },
    { archer: 'Archer Two', club: "Orion's", sex: 'Ladies', bowstyle: 'Traditional', score: 400, hits: 55, golds: 4 }
  ])
})

test('maps all requested club names, including both Glasgow spellings', () => {
  const aliases = [
    ['EK Archery Club', 'East Kilbride'], ["Orion's Archers", "Orion's"],
    ['Strathclyde AC', 'Strathclyde'], ['Glagow Archers', 'Glasgow'],
    ['Glasgow Archers', 'Glasgow'], ['UWS Archery Club', 'UWS'],
    ['Monklands AC', 'Monklands'], ['Linwood AC', 'Linwood'],
    ['Giffnock Archers', 'Giffnock']
  ]
  const rows = aliases.map(([club], i) => [`${i + 1}A`, `Archer ${i}`, club, 'O', 'Recurve', 500, 60, 10])
  assert.deepEqual(parseResultsSheet(workbook(rows)).archers.map((r) => r.club), aliases.map(([, club]) => club))
})

test('preserves unrecognized values for review and normalizes short bow codes', () => {
  const rows = [
    ['1A', 'Unknown', 'Other Club', 'X', 'Mystery', 400, 60, 2],
    ['1B', 'Longbow', 'Linwood AC', 'F', 'LB', 420, 60, 2]
  ]
  const [unknown, longbow] = parseResultsSheet(workbook(rows)).archers
  assert.equal(unknown.club, 'Other Club')
  assert.equal(unknown.sex, null)
  assert.equal(unknown.sexUnknown, true)
  assert.equal(unknown.bowstyle, 'Mystery')
  assert.equal(unknown.bowstyleUnknown, true)
  assert.equal(longbow.bowstyle, 'Traditional')
  assert.equal(longbow.sex, 'Ladies')
})

test('rejects missing Results sheets and sheets without scored archers', () => {
  assert.throws(() => parseResultsSheet(workbook([['1A', 'A', 'Club', 'O', 'RC', 1, 1, 1]], 'Team Scores')), /Results/i)
  assert.throws(() => parseResultsSheet(workbook([['1A', 'A', 'Club', 'O', 'RC', '', '', '']])), /score/i)
  assert.throws(() => parseResultsSheet(workbook([['1A', 'A', 'Club', 'O', 'RC', 0, 0, 0]])), /score/i)
})

test('rejects malformed score values and reports their worksheet row', () => {
  assert.throws(
    () => parseResultsSheet(workbook([['1A', 'Archer', 'Club', 'O', 'RC', 'not a score', 60, 1]])),
    /invalid score.*row 3/i
  )
})

test('rejects scored rows missing an archer rather than silently omitting them', () => {
  assert.throws(
    () => parseResultsSheet(workbook([
      ['1A', 'Valid', 'Glasgow Archers', 'O', 'RC', 500, 60, 10],
      ['1B', '', 'Glasgow Archers', 'O', 'RC', 400, 60, 5]
    ])),
    /archer.*row 4/i
  )
})

test('club and bowstyle changes create separate records without changing previous results', async () => {
  assert.equal(typeof importService.saveImportedScores, 'function')
  const archers = [{ id: 1, archer: 'Known', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Open' }]
  let scores = [{ archer_id: 1, event_id: 9, score: 555, hits: 60, golds: 12 }]
  const client = {
    async query(sql, params = []) {
      if (['BEGIN', 'COMMIT', 'ROLLBACK'].includes(sql)) return {}
      if (sql.startsWith('SELECT id, archer, club, bowstyle FROM archers')) return { rows: [...archers] }
      if (sql.startsWith('INSERT INTO archers')) {
        const [archer, club, bowstyle, sex] = params
        const row = { id: archers.length + 1, archer, club, bowstyle, sex }
        archers.push(row)
        return { rows: [{ id: row.id }] }
      }
      if (sql.startsWith('DELETE FROM event_scores')) {
        scores = scores.filter((s) => s.event_id !== params[0])
        return {}
      }
      if (sql.startsWith('INSERT INTO event_scores')) {
        const [archer_id, event_id, score, hits, golds] = params
        scores.push({ archer_id, event_id, score, hits, golds })
        return {}
      }
      throw new Error(`Unexpected query: ${sql}`)
    }
  }
  const result = await importService.saveImportedScores(client, 13, [
    { archer: 'Known', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Open', score: 500, hits: 60, golds: 10, isNew: false, archer_id: 1 },
    { archer: 'Known', club: 'Linwood', bowstyle: 'Recurve', sex: 'Open', score: 490, hits: 60, golds: 9, isNew: true },
    { archer: 'Known', club: 'Glasgow', bowstyle: 'Compound', sex: 'Open', score: 530, hits: 60, golds: 15, isNew: true }
  ])
  assert.deepEqual(result, { newArchers: 2, scoresInserted: 3 })
  assert.equal(archers.length, 3)
  assert.deepEqual(archers[0], { id: 1, archer: 'Known', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Open' })
  assert.deepEqual(scores.find((s) => s.event_id === 9), { archer_id: 1, event_id: 9, score: 555, hits: 60, golds: 12 })
  assert.deepEqual(scores.filter((s) => s.event_id === 13).map((s) => s.archer_id), [1, 2, 3])
})

test('rolls back the import transaction if score insertion fails', async () => {
  const statements = []
  const client = {
    async query(sql) {
      statements.push(sql)
      if (sql.startsWith('SELECT id')) return { rows: [{ id: 1, archer: 'Known', club: 'Glasgow', bowstyle: 'Recurve' }] }
      if (sql.startsWith('INSERT INTO event_scores')) throw new Error('score insert failed')
      return {}
    }
  }
  await assert.rejects(importService.saveImportedScores(client, 13, [
    { archer: 'Known', club: 'Glasgow', bowstyle: 'Recurve', archer_id: 1, score: 500, hits: 60, golds: 10 }
  ]), /score insert failed/)
  assert.equal(statements.at(-1), 'ROLLBACK')
  assert.ok(!statements.includes('COMMIT'))
})

test('preview retains existing sex, flags mismatch and fills new archer sex', async () => {
  const originalQuery = pool.query
  pool.query = async (sql) => {
    if (sql.includes('FROM archers')) return { rows: [{ id: 1, archer: 'Known', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Ladies' }] }
    if (sql.includes('FROM events')) return { rows: [{ id: 13, venue_name: 'Test' }] }
    if (sql.includes('FROM event_scores')) return { rows: [{ archer_id: 1 }] }
    throw new Error(`Unexpected query: ${sql}`)
  }
  const app = express()
  app.use(express.json())
  app.use('/import', importRoutes)
  const server = app.listen(0, 'localhost')
  await once(server, 'listening')
  const url = `http://localhost:${server.address().port}/import`
  try {
    const response = await preview(url, workbook([
      ['1A', 'Known', 'Glasgow Archers', 'O', 'Recurve', 500, 60, 10],
      ['1B', 'New', 'Monklands AC', 'F', 'Longbow', 400, 55, 4],
      ['1C', 'Known', 'Other Club', 'O', 'Recurve', 450, 60, 3],
      ['1D', 'Knwon', 'Glasgow Archers', 'O', 'Recurve', 470, 60, 4]
    ]))
    assert.equal(response.status, 200)
    const body = await response.json()
    assert.deepEqual(body.counts, { known: 1, new: 3, total: 4 })
    assert.equal(body.knownScores[0].sex, 'Ladies')
    assert.equal(body.knownScores[0].sheetSex, 'Open')
    assert.equal(body.knownScores[0].sexMismatch, true)
    assert.equal(body.knownScores[0].alreadyImported, true)
    assert.equal(body.newArchers[0].sex, 'Ladies')
    assert.equal(body.newArchers[0].bowstyle, 'Traditional')
    assert.deepEqual(body.archerCatalog, [
      { archer_id: 1, archer: 'Known', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Ladies', alreadyImported: true }
    ])
    assert.deepEqual(body.newArchers[1].existingCandidates, [
      { club: 'Glasgow', bowstyle: 'Recurve', sex: 'Ladies', alreadyImported: true }
    ])
    const corrected = prepareImportRow({ ...body.newArchers[2], archer: 'Known' }, body.archerCatalog)
    assert.equal(corrected.archer_id, 1)
    assert.equal(corrected.sex, 'Ladies')
    assert.equal(corrected.isNew, false)
    assert.equal(corrected.score, 470)

    const badPreview = await preview(url, workbook([], 'Team Scores'))
    assert.equal(badPreview.status, 400)
    assert.match((await badPreview.json()).error, /Results/i)

    for (const rows of [[], [{ archer: 'Blank', isNew: true, sex: 'Open', score: 0, hits: 0, golds: 0 }]]) {
      const commit = await fetch(`${url}/commit`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ event_id: 13, rows })
      })
      assert.equal(commit.status, 400)
      assert.match((await commit.json()).error, /score/i)
    }
    for (const row of [
      { archer: 'New', club: 'Glasgow', bowstyle: 'Recurve', sex: 'Gents', score: 500, isNew: true },
      { archer: 'New', club: 'Unknown', bowstyle: 'Recurve', sex: 'Open', score: 500, isNew: true },
      { archer: 'New', club: 'Glasgow', bowstyle: 'Unknown', sex: 'Open', score: 500, isNew: true }
    ]) {
      const commit = await fetch(`${url}/commit`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ event_id: 13, rows: [row] })
      })
      assert.equal(commit.status, 400)
    }
  } finally {
    pool.query = originalQuery
    await new Promise((resolve) => server.close(resolve))
  }
})
