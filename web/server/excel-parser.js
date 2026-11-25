// Parse scored archer rows from the current monthly Results worksheet.
// Its Archer and Club header labels are reversed relative to the data cells.
import XLSX from 'xlsx'

const BOWSTYLE = {
  rc: 'Recurve', rec: 'Recurve', recurve: 'Recurve',
  c: 'Compound', com: 'Compound', compound: 'Compound',
  bb: 'Barebow', barebow: 'Barebow',
  trad: 'Traditional', tradit: 'Traditional', traditional: 'Traditional',
  lb: 'Traditional', longbow: 'Traditional'
}

const CLUB = {
  'ek archery club': 'East Kilbride',
  "orion's archers": "Orion's",
  orions: "Orion's",
  "orion's": "Orion's",
  orion: "Orion's",
  'strathclyde ac': 'Strathclyde',
  'glagow archers': 'Glasgow',
  'glasgow archers': 'Glasgow',
  'uws archery club': 'UWS',
  'monklands ac': 'Monklands',
  'linwood ac': 'Linwood',
  'giffnock archers': 'Giffnock'
}

const SEX = { o: 'Open', open: 'Open', f: 'Ladies', ladies: 'Ladies' }

/** Trim spreadsheet values and normalize missing cells to empty strings. */
function norm(value) {
  return value === null || value === undefined ? '' : String(value).trim()
}

/** Parse an optional non-negative integer without silently losing invalid data. */
function count(value, field, rowNumber) {
  const text = norm(value)
  if (!text) return 0
  if (!/^\d+$/.test(text)) throw new Error(`Invalid ${field} in Results row ${rowNumber}`)
  return Number(text)
}

/** Read scored Results rows, preserving unknown categories for manual review. */
export function parseResultsSheet(buffer) {
  const wb = XLSX.read(buffer, { type: 'buffer' })
  const sheetName = wb.SheetNames.find((name) => /^results$/i.test(name.trim()))
  if (!sheetName) throw new Error('Workbook must contain a Results sheet')

  const sheet = wb.Sheets[sheetName]
  const rowOffset = sheet['!ref'] ? XLSX.utils.decode_range(sheet['!ref']).s.r : 0
  const rows = XLSX.utils.sheet_to_json(sheet, { header: 1, raw: false, defval: '' })
  const headerRow = rows.findIndex((row) => row.some((cell, col) =>
    /^target$/i.test(norm(cell)) && /^bow\s*type$/i.test(norm(row[col + 4])) &&
    /^score$/i.test(norm(row[col + 5])) && /^hits$/i.test(norm(row[col + 6]))
  ))
  if (headerRow < 0) throw new Error('Results sheet has an unsupported column layout')
  const start = rows[headerRow].findIndex((cell) => /^target$/i.test(norm(cell)))

  const archers = []
  for (let i = headerRow + 1; i < rows.length; i++) {
    const row = rows[i]
    const archer = norm(row[start + 1])
    const club = norm(row[start + 2])
    const sex = norm(row[start + 3])
    const bowstyle = norm(row[start + 4])
    const rowNumber = rowOffset + i + 1
    const score = count(row[start + 5], 'score', rowNumber)
    const hits = count(row[start + 6], 'hits', rowNumber)
    const golds = count(row[start + 7], 'tens', rowNumber)
    if (!score && !hits && !golds) continue
    if (!archer || archer === '0') throw new Error(`Missing archer in Results row ${rowNumber}`)

    const result = {
      archer,
      club: CLUB[club.toLowerCase()] || club,
      sex: SEX[sex.toLowerCase()] || null,
      bowstyle: BOWSTYLE[bowstyle.toLowerCase()] || bowstyle,
      score, hits, golds
    }
    if (sex && !result.sex) result.sexUnknown = true
    if (!BOWSTYLE[bowstyle.toLowerCase()]) result.bowstyleUnknown = true
    archers.push(result)
  }
  if (!archers.length) throw new Error('Results sheet contains no scored archers')
  return { meta: { title: '', venue: '', organiser: '', sheetName }, archers }
}
