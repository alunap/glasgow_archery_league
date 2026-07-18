// Parse the monthly "Team Scores" Excel sheet into structured archer rows.
// Layout (per-club blocks):
//   row N:   ["", "<Club>", "", "Score", "Hits", "Tens", ...]   <- club header
//   row N+1: ["<Archer>", "", "<Bowcode>", score, hits, tens]   <- archer row
//   ... repeats per club. No sex column. tens -> golds.
// Bowstyle codes: RC=Recurve, C=Compound, BB=Barebow, TRAD=Traditional, LB=Longbow.
import XLSX from 'xlsx'

const BOWSTYLE = {
  rc: 'Recurve', rec: 'Recurve', recurve: 'Recurve',
  c: 'Compound', com: 'Compound', compound: 'Compound',
  bb: 'Barebow', barebow: 'Barebow',
  trad: 'Traditional', tradit: 'Traditional', traditional: 'Traditional',
  lb: 'Longbow', longbow: 'Longbow'
}

const CLUB = {
  orions: "Orion's",
  'orion\'s': "Orion's",
  'orion': "Orion's"
}

function norm(v) {
  if (v === null || v === undefined) return ''
  return String(v).trim()
}

export function parseResultsSheet(buffer) {
  const wb = XLSX.read(buffer, { type: 'buffer' })
  // Prefer "Team Scores"; fall back to first sheet.
  const sheetName = wb.SheetNames.find((n) => /team scores/i.test(n)) || wb.SheetNames[0]
  const sheet = wb.Sheets[sheetName]
  const rows = XLSX.utils.sheet_to_json(sheet, { header: 1, raw: false, defval: '' })

  const archers = []
  const meta = { title: '', venue: '', organiser: '', sheetName }
  if (rows[0]) meta.title = norm(rows[0][0])
  if (rows[1]) meta.venue = norm(rows[1][0])
  if (rows[2]) meta.organiser = norm(rows[2][0])

  let currentClub = null
  for (let i = 0; i < rows.length; i++) {
    const r = rows[i]
    const a = norm(r[0]) // col A: archer name (or title row)
    const b = norm(r[1]) // col B: club name on header rows
    const c = norm(r[2]) // col C: bowstyle code
    const d = norm(r[3]) // col D: score (or "Score" label on header)
    const e = norm(r[4]) // col E: hits
    const f = norm(r[5]) // col F: tens

    // Club header: col B non-empty AND col D is "Score" label
    if (b && /score/i.test(d)) {
      currentClub = CLUB[b.toLowerCase()] || b
      continue
    }
    // Archer row: col A non-empty (a name) AND col C non-empty (a bowstyle code)
    if (a && c) {
      const bow = BOWSTYLE[c.toLowerCase()]
      if (!bow) {
        // unknown bowstyle code — still record with raw code so UI can flag it
        archers.push({
          archer: a, club: currentClub, bowstyle: c, bowstyleUnknown: true,
          score: Number(d) || 0, hits: Number(e) || 0, golds: Number(f) || 0
        })
        continue
      }
      archers.push({
        archer: a, club: currentClub, bowstyle: bow,
        score: Number(d) || 0, hits: Number(e) || 0, golds: Number(f) || 0
      })
    }
  }
  return { meta, archers }
}
