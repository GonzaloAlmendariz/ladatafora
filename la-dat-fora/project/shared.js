// Shared layout fragments — masthead, footer, tweaks panel, reveal + mini chart helpers
// Pure vanilla; no framework needed. v2

(function () {
  const PAGES = [
    { href: 'index.html',     label: 'Portada',    key: 'home' },
    { href: 'cv.html',        label: 'Perfil',     key: 'cv' },
    { href: 'prosecnur.html', label: 'Prosecnur',  key: 'prosecnur' },
  ];

  function esc(s){ return String(s).replace(/[&<>"]/g, m => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[m])); }

  function today() {
    const d = new Date();
    const ms = ['ENE','FEB','MAR','ABR','MAY','JUN','JUL','AGO','SEP','OCT','NOV','DIC'];
    return `${String(d.getDate()).padStart(2,'0')} ${ms[d.getMonth()]} ${d.getFullYear()}`;
  }

  window.LD = {
    renderMasthead(active) {
      const nav = PAGES.map(p =>
        `<a href="${p.href}" class="${p.key === active ? 'active' : ''}">${p.label}</a>`
      ).join('');
      return `
        <header class="masthead">
          <div class="wrap masthead-row">
            <a class="mast-brand" href="index.html" aria-label="La Datáfora">
              <span>La <em>Datáfora</em></span>
              <span class="vol">VOL. 01</span>
            </a>
            <nav class="mast-nav">${nav}</nav>
            <div class="mast-meta"><span class="dot"></span>LIMA · ${today()}</div>
          </div>
        </header>
        <div class="wrap">
          <div class="issue-strip">
            <div class="left">Gonzalo Almendariz Villanueva</div>
            <div class="center">Un cuaderno de análisis de datos aplicado</div>
            <div class="right">ESP · ES-PE</div>
          </div>
        </div>
      `;
    },

    renderFooter() {
      return `
        <footer class="foot">
          <div class="wrap">
            <div class="foot-grid">
              <div>
                <h4>La Datáfora</h4>
                <p style="color:var(--ink-soft); line-height:1.6; max-width:44ch;">
                  Trabajo, ideas y herramientas sobre análisis de datos en ciencias sociales.
                  Publicaciones, materiales didácticos y paquetes de R con enfoque reproducible.
                </p>
              </div>
              <div>
                <h4>Navegar</h4>
                <a href="index.html">Portada</a>
                <a href="cv.html">Perfil</a>
                <a href="prosecnur.html">Prosecnur</a>
                <a href="#">IncoreR</a>
                <a href="#">El libro</a>
              </div>
              <div>
                <h4>Producido en</h4>
                <a href="#">R · tidyverse</a>
                <a href="#">Quarto</a>
                <a href="#">Shiny</a>
                <a href="#">XLSForm · KoBo</a>
              </div>
              <div>
                <h4>Contacto</h4>
                <a href="mailto:gonzaloalmendariz@gmail.com">correo</a>
                <a href="https://github.com/GonzaloAlmendariz" target="_blank" rel="noopener">github</a>
                <a href="#">linkedin</a>
                <a href="#">rpubs</a>
              </div>
            </div>

            <div class="foot-mark" aria-hidden="true">La <em>Datáfora</em> &mdash; Vol. 01</div>

            <div class="foot-colophon">
              <span>© 2026 · Elaborado por Gonzalo Almendariz Villanueva</span>
              <span>Compuesto en Instrument Serif, Inter Tight &amp; JetBrains Mono</span>
            </div>
          </div>
        </footer>
      `;
    },

    renderTweaks() {
      return `
        <aside class="tweaks" id="ldTweaks" role="region" aria-label="Tweaks">
          <h5>
            <span>Ajustes</span>
            <span class="x" id="ldTweaksClose" aria-label="Cerrar">×</span>
          </h5>
          <div class="tweak-row">
            <label>Hero</label>
            <div class="opts" data-tweak="hero">
              <button data-v="type">type</button>
              <button data-v="data">data</button>
              <button data-v="image">image</button>
            </div>
          </div>
          <div class="tweak-row">
            <label>Ritmo</label>
            <div class="opts two" data-tweak="rhythm">
              <button data-v="asymmetric">asym</button>
              <button data-v="centered">centrado</button>
            </div>
          </div>
          <div class="tweak-row">
            <label>Densidad</label>
            <div class="opts two" data-tweak="density">
              <button data-v="airy">airy</button>
              <button data-v="compact">compact</button>
            </div>
          </div>
        </aside>
      `;
    },

    mountChrome(active) {
      document.getElementById('masthead-slot').innerHTML = this.renderMasthead(active);
      document.getElementById('footer-slot').innerHTML   = this.renderFooter();
      document.getElementById('tweaks-slot').innerHTML   = this.renderTweaks();
      this.wireReveal();
      this.wireTweaks();
    },

    wireReveal() {
      const io = new IntersectionObserver((entries)=>{
        entries.forEach(e=>{
          if (e.isIntersecting) {
            e.target.classList.add('is-visible');
            io.unobserve(e.target);
          }
        });
      }, { threshold: 0.08, rootMargin: '0px 0px -6% 0px' });
      document.querySelectorAll('.reveal').forEach(el => io.observe(el));
    },

    wireTweaks() {
      const root = document.documentElement;
      const tw   = document.getElementById('ldTweaks');
      const defaults = window.TWEAK_DEFAULTS || { hero: 'data', rhythm: 'asymmetric', density: 'airy' };
      const state = Object.assign({}, defaults);

      const apply = () => {
        root.setAttribute('data-hero',    state.hero);
        root.setAttribute('data-rhythm',  state.rhythm);
        root.setAttribute('data-density', state.density);
        tw.querySelectorAll('.opts').forEach(group => {
          const key = group.dataset.tweak;
          group.querySelectorAll('button').forEach(b => {
            b.classList.toggle('on', b.dataset.v === state[key]);
          });
        });
      };
      apply();

      tw.addEventListener('click', (e) => {
        const b = e.target.closest('button[data-v]');
        if (!b) return;
        const key = b.parentElement.dataset.tweak;
        state[key] = b.dataset.v;
        apply();
        try {
          window.parent.postMessage({ type: '__edit_mode_set_keys', edits: { [key]: state[key] } }, '*');
        } catch(_) {}
      });

      document.getElementById('ldTweaksClose').addEventListener('click', () => {
        tw.classList.remove('visible');
        try { window.parent.postMessage({ type: '__edit_mode_deactivated' }, '*'); } catch(_) {}
      });

      // host protocol
      window.addEventListener('message', (e) => {
        const d = e && e.data;
        if (!d || typeof d !== 'object') return;
        if (d.type === '__activate_edit_mode')   tw.classList.add('visible');
        if (d.type === '__deactivate_edit_mode') tw.classList.remove('visible');
      });
      try { window.parent.postMessage({ type: '__edit_mode_available' }, '*'); } catch(_) {}
    },

    // ---------- Tiny SVG charts that react to hover ----------

    // Sparkline given values 0..1
    sparkline(values, { width=240, height=48, stroke='var(--ink)' } = {}) {
      const step = width / (values.length - 1);
      const pts  = values.map((v,i) => `${(i*step).toFixed(1)},${(height - v*height).toFixed(1)}`).join(' ');
      return `
        <svg class="sparkline" viewBox="0 0 ${width} ${height}" width="100%" preserveAspectRatio="none" aria-hidden="true">
          <polyline fill="none" stroke="${stroke}" stroke-width="1.25" points="${pts}" />
          <circle class="sparkline-dot" r="3" fill="${stroke}" cx="${(width - 1).toFixed(1)}" cy="${(height - values[values.length-1]*height).toFixed(1)}"/>
        </svg>
      `;
    },

    // Tiny bars
    bars(values, { width=240, height=48, fill='var(--ink)' } = {}) {
      const gap = 2;
      const n = values.length;
      const bw = (width - gap*(n-1)) / n;
      return `
        <svg class="barchart" viewBox="0 0 ${width} ${height}" width="100%" preserveAspectRatio="none" aria-hidden="true">
          ${values.map((v,i) => {
            const h = v * height;
            const x = i * (bw + gap);
            return `<rect x="${x.toFixed(1)}" y="${(height-h).toFixed(1)}" width="${bw.toFixed(1)}" height="${h.toFixed(1)}" fill="${fill}"/>`;
          }).join('')}
        </svg>
      `;
    },

    // Scatter for hover interaction
    scatter(points, { width=240, height=140 } = {}) {
      return `
        <svg class="scatter" viewBox="0 0 ${width} ${height}" width="100%" aria-hidden="true">
          ${points.map(p => `<circle cx="${(p.x*width).toFixed(1)}" cy="${((1-p.y)*height).toFixed(1)}" r="${p.r||2.8}" fill="var(--ink)" opacity="${p.o||0.72}"/>`).join('')}
        </svg>
      `;
    },

    // Animate sparklines on hover within a root
    animateOnHover(rootSel) {
      document.querySelectorAll(rootSel).forEach(root => {
        root.addEventListener('mouseenter', () => root.classList.add('is-live'));
        root.addEventListener('mouseleave', () => root.classList.remove('is-live'));
      });
    }
  };
})();
