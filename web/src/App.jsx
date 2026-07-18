import { Routes, Route, NavLink } from 'react-router-dom'
import Home from './pages/Home.jsx'
import Import from './pages/Import.jsx'
import Archers from './pages/Archers.jsx'
import Venues from './pages/Venues.jsx'
import Events from './pages/Events.jsx'
import Scores from './pages/Scores.jsx'
import Badges from './pages/Badges.jsx'

const link = ({ isActive }) => 'nav-link' + (isActive ? ' active' : '')

export default function App() {
  return (
    <div className="app">
      <header className="topbar">
        <div className="brand">Glasgow League Admin</div>
        <nav className="nav">
          <NavLink to="/" end className={link}>Home</NavLink>
          <NavLink to="/import" className={link}>Import</NavLink>
          <NavLink to="/archers" className={link}>Archers</NavLink>
          <NavLink to="/venues" className={link}>Venues</NavLink>
          <NavLink to="/events" className={link}>Events</NavLink>
          <NavLink to="/scores" className={link}>Scores</NavLink>
          <NavLink to="/badges" className={link}>Badges</NavLink>
        </nav>
      </header>
      <main className="content">
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="/import" element={<Import />} />
          <Route path="/archers" element={<Archers />} />
          <Route path="/venues" element={<Venues />} />
          <Route path="/events" element={<Events />} />
          <Route path="/scores" element={<Scores />} />
          <Route path="/badges" element={<Badges />} />
        </Routes>
      </main>
    </div>
  )
}
