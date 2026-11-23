// Helpers for selecting an import event and matching reviewed archer identities.

/** Select the most recent unscored event on or before the local calendar date. */
export function selectDefaultEvent(events, now = new Date()) {
  const today = [
    now.getFullYear(),
    String(now.getMonth() + 1).padStart(2, '0'),
    String(now.getDate()).padStart(2, '0')
  ].join('-')
  const eligible = events
    .filter((event) => event.has_scores === false && /^\d{4}-\d{2}-\d{2}/.test(event.date_of_event || '') && event.date_of_event.slice(0, 10) <= today)
    .sort((a, b) => b.date_of_event.slice(0, 10).localeCompare(a.date_of_event.slice(0, 10)) || Number(b.id) - Number(a.id))
  return eligible.length ? String(eligible[0].id) : ''
}

/** Find an existing archer only when name, club and bowstyle all match. */
export function matchEditedArcher(row, catalog) {
  const name = row.archer.trim()
  return catalog.find((a) => a.archer === name && a.club === row.club && a.bowstyle === row.bowstyle) || null
}

/** Prepare an edited row for commit, retaining the identity and sex of a match. */
export function prepareImportRow(row, catalog) {
  const match = matchEditedArcher(row, catalog)
  return {
    archer: row.archer.trim(), club: row.club, bowstyle: row.bowstyle,
    sex: match ? match.sex : row.sex,
    score: row.score, hits: row.hits, golds: row.golds,
    ...(match ? { archer_id: match.archer_id, isNew: false } : { isNew: true })
  }
}
