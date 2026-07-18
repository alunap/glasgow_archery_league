const BASE = import.meta.env.VITE_API_URL || '/api'

async function request(path, opts = {}) {
  const res = await fetch(`${BASE}${path}`, {
    headers: opts.body && !(opts.body instanceof FormData) ? { 'Content-Type': 'application/json' } : {},
    ...opts,
    body: opts.body instanceof FormData ? opts.body : (opts.body ? JSON.stringify(opts.body) : undefined)
  })
  const text = await res.text()
  const data = text ? JSON.parse(text) : null
  if (!res.ok) throw new Error(data?.error || `HTTP ${res.status}`)
  return data
}

export const api = {
  health: () => request('/health'),
  sync: () => request('/sync', { method: 'POST' }),
  archers: (q = '') => request(`/archers${q}`),
  venues: () => request('/venues'),
  events: () => request('/events'),
  scores: (q = '') => request(`/scores${q}`),
  badges: () => request('/badges'),
  importPreview: (formData) => request('/import/preview', { method: 'POST', body: formData }),
  importCommit: (payload) => request('/import/commit', { method: 'POST', body: payload })
}

export const crud = (resource) => ({
  list: (q = '') => request(`/${resource}${q}`),
  create: (body) => request(`/${resource}`, { method: 'POST', body }),
  update: (id, body) => request(`/${resource}${id ? '/' + id : ''}`, { method: 'PUT', body }),
  remove: (id) => request(`/${resource}${id ? '/' + id : ''}`, { method: 'DELETE' })
})

export default api
