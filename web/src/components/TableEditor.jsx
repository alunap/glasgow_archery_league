import { useState, useMemo } from 'react'

// columns: [{ key, label, editable, type, options, width, render }]
// idKey: name of the id field (for update/delete URLs). If null, uses composite body.
// onAdd(row), onSave(id, row), onDelete(id). rowState provided by parent for controlled filters.
export default function TableEditor({
  rows, columns, idKey = 'id',
  onAdd, onSave, onDelete,
  addLabel = 'Add row',
  emptyLabel = 'No rows',
  rowClass
}) {
  const [sort, setSort] = useState({ key: null, dir: 1 })
  const [draft, setDraft] = useState(null)   // {mode:'add'|'edit', row}
  const [blank, setBlank] = useState({})     // new-row input buffer

  const sorted = useMemo(() => {
    if (!sort.key) return rows
    const k = sort.key
    return [...rows].sort((a, b) => {
      const av = a[k] ?? ''
      const bv = b[k] ?? ''
      if (typeof av === 'number' && typeof bv === 'number') return (av - bv) * sort.dir
      return String(av).localeCompare(String(bv)) * sort.dir
    })
  }, [rows, sort])

  const toggleSort = (key) => {
    setSort((s) => (s.key === key ? { key, dir: -s.dir } : { key, dir: 1 }))
  }

  const startEdit = (row) => setDraft({ mode: 'edit', row: { ...row } })
  const startAdd = () => {
    const b = {}
    columns.filter((c) => c.editable).forEach((c) => (b[c.key] = c.default ?? ''))
    setBlank(b)
    setDraft({ mode: 'add', row: b })
  }
  const setField = (key, val) => setDraft((d) => ({ ...d, row: { ...d.row, [key]: val } }))

  const cancel = () => setDraft(null)
  const save = async () => {
    try {
      if (draft.mode === 'add') await onAdd(draft.row)
      else await onSave(draft.row[idKey], draft.row)
      setDraft(null)
    } catch (e) {
      alert(e.message)
    }
  }
  const remove = async (row) => {
    if (!confirm(`Delete ${row[idKey]}?`)) return
    try { await onDelete(row[idKey]) } catch (e) { alert(e.message) }
  }

  return (
    <div>
      <div className="toolbar">
        <button className="primary" onClick={startAdd} disabled={!onAdd}>{addLabel}</button>
        {sort.key && <span className="muted">sorted by {sort.key} {sort.dir > 0 ? '↑' : '↓'}</span>}
      </div>
      <table className="editable">
        <thead>
          <tr>
            {columns.map((c) => (
              <th
                key={c.key}
                className={sort.key === c.key ? 'sorted' : ''}
                data-order={sort.dir > 0 ? '↑' : '↓'}
                onClick={() => toggleSort(c.key)}
                style={c.width ? { width: c.width } : undefined}
              >
                {c.label}
              </th>
            ))}
            <th style={{ width: '180px' }}>Actions</th>
          </tr>
        </thead>
        <tbody>
          {sorted.length === 0 && (
            <tr><td colSpan={columns.length + 1} className="muted">{emptyLabel}</td></tr>
          )}
          {sorted.map((row) => {
            const isEditing = draft?.mode === 'edit' && draft.row[idKey] === row[idKey]
            const cls = rowClass ? rowClass(row) : ''
            return (
              <tr key={row[idKey] ?? JSON.stringify(row)} className={cls}>
                {columns.map((c) => (
                  <td key={c.key} className={isEditing && c.editable ? 'cell-edit' : ''}>
                    {isEditing && c.editable ? (
                      c.type === 'select' ? (
                        <select value={draft.row[c.key] ?? ''} onChange={(e) => setField(c.key, e.target.value)}>
                          <option value="">—</option>
                          {c.options.map((o) => <option key={o} value={o}>{o}</option>)}
                        </select>
                      ) : (
                        <input
                          type={c.type === 'number' ? 'number' : c.type === 'date' ? 'date' : 'text'}
                          value={draft.row[c.key] ?? ''}
                          onChange={(e) => setField(c.key, e.target.value)}
                        />
                      )
                    ) : c.render ? c.render(row) : (row[c.key] ?? '')}
                  </td>
                ))}
                <td>
                  {isEditing ? (
                    <span className="row gap-sm">
                      <button className="primary" onClick={save}>Save</button>
                      <button onClick={cancel}>Cancel</button>
                    </span>
                  ) : (
                    <span className="row gap-sm">
                      <button onClick={() => startEdit(row)} disabled={!onSave}>Edit</button>
                      <button className="danger" onClick={() => remove(row)} disabled={!onDelete}>Del</button>
                    </span>
                  )}
                </td>
              </tr>
            )
          })}
        </tbody>
      </table>
    </div>
  )
}
