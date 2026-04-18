// La Datáfora — UI fragments (masthead, footer, reveal, scroll progress, count-up)
(function () {
  const PAGES = [
    { href: 'index.html',     label: 'Inicio',     key: 'home' },
    { href: 'cv.html',        label: 'Perfil',     key: 'cv' },
    { href: 'libro.html',     label: 'Libro',      key: 'libro' },
    { href: 'incorer.html',   label: 'IncoreR',    key: 'incorer' },
    { href: 'prosecnur.html', label: 'Prosecnur',  key: 'prosecnur' },
  ];

  window.LD = {
    renderMasthead(active) {
      const nav = PAGES.map(p =>
        `<a href="${p.href}" class="${p.key === active ? 'active' : ''}">${p.label}</a>`
      ).join('');
      return `
        <header class="masthead" id="ldMast">
          <div class="wrap masthead-row">
            <a class="mast-brand" href="index.html" aria-label="La Datáfora">
              La <em>Datáfora</em>
            </a>
            <nav class="mast-nav">${nav}</nav>
            <a class="mast-cta" href="mailto:gonzaloalmendariz@gmail.com">Contacto</a>
          </div>
        </header>
        <div class="scroll-progress" id="ldProgress"></div>
      `;
    },

    renderFooter() {
      const year = new Date().getFullYear();
      return `
        <footer class="foot">
          <div class="wrap">
            <div class="foot-grid">
              <div>
                <div class="foot-brand">La <em>Datáfora</em></div>
                <p>
                  Herramientas en R, libros y proyectos aplicados al análisis de datos en
                  ciencias sociales. Enfoque reproducible, accesible y riguroso.
                </p>
              </div>
              <div>
                <h4>Navegar</h4>
                <a href="index.html">Inicio</a>
                <a href="cv.html">Perfil</a>
                <a href="libro.html">El libro</a>
                <a href="incorer.html">IncoreR</a>
                <a href="prosecnur.html">Prosecnur</a>
              </div>
              <div>
                <h4>Paquetes</h4>
                <a href="manual-incorer.html">Manual IncoreR</a>
                <a href="guia-prosecnur.html">Guía Prosecnur</a>
                <a href="https://github.com/GonzaloAlmendariz" target="_blank" rel="noopener">GitHub</a>
                <a href="https://rpubs.com/Gonzalo_Almendariz" target="_blank" rel="noopener">RPubs</a>
              </div>
              <div>
                <h4>Contacto</h4>
                <a href="mailto:gonzaloalmendariz@gmail.com">Correo</a>
                <a href="https://www.linkedin.com/in/gonzalo-almendariz-villanueva-051587324/" target="_blank" rel="noopener">LinkedIn</a>
                <a href="https://github.com/GonzaloAlmendariz" target="_blank" rel="noopener">GitHub</a>
              </div>
            </div>

            <div class="foot-bottom">
              <span>© ${year} · Gonzalo Almendariz Villanueva · Lima, Perú</span>
              <span>Hecho con HTML, CSS y atención a los detalles.</span>
            </div>
          </div>
        </footer>
      `;
    },

    mountChrome(active) {
      const mast = document.getElementById('masthead-slot');
      const foot = document.getElementById('footer-slot');
      if (mast) mast.innerHTML = this.renderMasthead(active);
      if (foot) foot.innerHTML = this.renderFooter();
      this.wireReveal();
      this.wireScroll();
      this.wireCountUp();
      this.wireSpotlight();
    },

    wireSpotlight() {
      document.querySelectorAll('.project-card').forEach(card => {
        card.addEventListener('pointermove', (e) => {
          const r = card.getBoundingClientRect();
          const mx = ((e.clientX - r.left) / r.width) * 100;
          const my = ((e.clientY - r.top) / r.height) * 100;
          card.style.setProperty('--mx', mx + '%');
          card.style.setProperty('--my', my + '%');
        });
      });
    },

    wireReveal() {
      const io = new IntersectionObserver((entries) => {
        entries.forEach(e => {
          if (e.isIntersecting) {
            e.target.classList.add('is-visible');
            io.unobserve(e.target);
          }
        });
      }, { threshold: 0.08, rootMargin: '0px 0px -6% 0px' });
      document.querySelectorAll('.reveal').forEach(el => io.observe(el));
    },

    wireScroll() {
      const mast = document.getElementById('ldMast');
      const bar = document.getElementById('ldProgress');
      let ticking = false;
      function onScroll() {
        ticking = false;
        const y = window.scrollY || 0;
        const h = document.documentElement.scrollHeight - window.innerHeight;
        if (mast) mast.classList.toggle('is-scrolled', y > 10);
        if (bar && h > 0) bar.style.width = Math.min(100, (y / h) * 100) + '%';
      }
      window.addEventListener('scroll', () => {
        if (!ticking) { requestAnimationFrame(onScroll); ticking = true; }
      }, { passive: true });
      onScroll();
    },

    wireCountUp() {
      const els = document.querySelectorAll('[data-count]');
      if (!els.length) return;
      const io = new IntersectionObserver((entries) => {
        entries.forEach(e => {
          if (!e.isIntersecting) return;
          const el = e.target;
          const target = parseFloat(el.dataset.count);
          const prefix = el.dataset.prefix || '';
          const suffix = el.dataset.suffix || '';
          const dur = 900;
          const t0 = performance.now();
          function step(t) {
            const p = Math.min(1, (t - t0) / dur);
            const eased = 1 - Math.pow(1 - p, 3);
            const v = target * eased;
            const out = Number.isInteger(target) ? Math.round(v) : v.toFixed(1);
            el.textContent = prefix + out + suffix;
            if (p < 1) requestAnimationFrame(step);
          }
          requestAnimationFrame(step);
          io.unobserve(el);
        });
      }, { threshold: 0.4 });
      els.forEach(el => io.observe(el));
    }
  };
})();
