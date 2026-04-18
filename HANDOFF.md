# Handoff Spec: La Datáfora

## Overview

**La Datáfora** es el cuaderno abierto de Gonzalo Almendariz Villanueva: paquetes de R, materiales didácticos y proyectos aplicados al análisis de datos en ciencias sociales. El sitio es un cuaderno editorial estático (data-journal) que reúne cinco landings + dos manuales largos migrados desde Quarto.

- **Stack**: HTML + CSS + Vanilla JS (sin framework, sin build).
- **Despliegue**: GitHub Pages sirve `docs/` de la rama `main`.
- **Idioma**: Español latinoamericano estándar (sin voseo).
- **Tono**: Editorial — tipografía serif con cursivas para énfasis, reglas horizontales fuertes, paletas de papel/tinta, detalles monoespaciados para metadatos.

### Archivos

| Ruta | Rol |
|---|---|
| `docs/index.html` | Portada |
| `docs/cv.html` | Perfil |
| `docs/libro.html` | Libro + Datáfora Interactiva |
| `docs/incorer.html` | Landing paquete IncoreR |
| `docs/prosecnur.html` | Landing paquete Prosecnur |
| `docs/manual-incorer.html` | Manual largo IncoreR (migrado) |
| `docs/guia-prosecnur.html` | Guía larga Prosecnur (migrado) |
| `docs/shared.css` | Tokens + componentes |
| `docs/shared.js` | Masthead/footer dinámicos, reveal, mini-charts |
| `docs/Manual_files/` · `docs/Guia_Prosecnur_files/` | Figuras R renderizadas |

---

## Design Tokens

Todos los tokens viven en `:root` de `shared.css`. Úsalos siempre — nunca literales de color/spacing.

### Colores (papel / tinta / acentos)

| Token | Valor | Uso |
|---|---|---|
| `--paper` | `#F2EFE7` | Fondo principal del sitio |
| `--paper-deep` | `#E9E4D6` | Bloques con contraste suave (covers, dint) |
| `--paper-soft` | `#EDE9DD` | Hover rows, bloques secundarios |
| `--ink` | `#121212` | Texto principal, botones sólidos, bloques de código |
| `--ink-soft` | `#2A2826` | Texto de cuerpo / párrafos |
| `--ink-mute` | `#6A665E` | Metadatos, captions, labels secundarios |
| `--rule` | `#1210120F` | Hairline dashed/soft |
| `--rule-strong` | `#12101233` | Rule divisor estándar |
| `--accent` | `oklch(0.58 0.14 35)` | Terracotta — cursivas editoriales, links en `.doc`, énfasis |
| `--accent-soft` | `oklch(0.86 0.05 35)` | Variante clara sobre fondos oscuros |
| `--ink-blue` | `oklch(0.48 0.09 240)` | Reservado para charts |
| `--signal` | `oklch(0.72 0.14 135)` | Status "OK"/"activo"; dot pulsante del masthead |

### Tipografía

| Token | Familia | Carga |
|---|---|---|
| `--serif` | Instrument Serif → Fraunces → Georgia | Google Fonts (+italic). Títulos, cursivas, covers |
| `--sans` | Inter Tight → Inter → system-ui | Google Fonts. Cuerpo, UI, chips |
| `--mono` | JetBrains Mono → ui-monospace → Menlo | Google Fonts. Metadatos, código, kickers |

### Escala tipográfica (clamp fluido)

| Token | Min | Max | Uso |
|---|---|---|---|
| `--step--1` | .78rem | .86rem | Captions, micro |
| `--step-0` | .95rem | 1.05rem | Cuerpo default |
| `--step-1` | 1.1rem | 1.25rem | Lede, párrafos destacados |
| `--step-2` | 1.4rem | 1.8rem | h3 secundarios |
| `--step-3` | 2rem | 2.9rem | h2 de sección |
| `--step-4` | 2.8rem | 4.8rem | Títulos hero medios |
| `--step-5` | 4rem | 8.5rem | Hero title XL (home, paquetes) |

### Spacing / densidad

| Token | Default | Uso |
|---|---|---|
| `--pad-section` | `clamp(64px, 8vw, 112px)` | Vertical entre secciones |
| `--pad-inline` | `clamp(20px, 4vw, 44px)` | Padding horizontal del `.wrap` |
| `--gutter` | `clamp(16px, 2.4vw, 28px)` | Gap entre columnas de grids |

`[data-density="compact"]` reduce todos los paddings ~35%. No activo por defecto.

### Letter-spacing / family-rules convencionales

- Kickers / labels uppercase → `var(--mono)`, `letter-spacing: 0.14em`, `text-transform: uppercase`, `.72rem`
- Títulos serif → `font-weight: 400` (nunca bold), `letter-spacing: -0.02em` a `-0.035em` según tamaño
- Sans-serif body → `font-weight: 400-500`, chips/tags `500-600`

---

## Layout System

### Grid container

```
<main class="wrap"> … </main>
```

`.wrap` → `max-width: 1320px`, `padding-inline: var(--pad-inline)`, centrado. `.wrap-narrow` = 960px para largo texto.

### Breakpoints

| Breakpoint | Cambios clave |
|---|---|
| **≥1100px** | Layout columnas completas; TOC flotante visible en `.doc` |
| **901–1099px** | Sigue grid pero empieza a comprimir hero sidebars |
| **781–900px** | `.hero-grid`, `.cv-hero`, `.ph` pasan a 1 col; `.now`, `.signals`, `.contact`, `.install` a 1 col |
| **≤780px** | Masthead colapsa: nav en segunda fila scroll-x; `.mast-meta` oculto; `.foot-grid` a 2 cols |
| **≤700px** | `.tl` timeline pierde columna `tl-when`, va arriba |

### Estructura canónica de página

```html
<body>
  <div id="masthead-slot"></div>       <!-- inyecta shared.js -->
  <main class="wrap">
    <section class="[hero-variant] reveal">…</section>
    <section class="reveal">
      <div class="section-head">
        <div class="num-label">§ 01 / Categoría</div>
        <div>
          <h2>Título</h2>
          <p class="lede">Bajada.</p>
        </div>
      </div>
      <!-- Contenido de sección -->
    </section>
    …
  </main>
  <div id="footer-slot"></div>
  <script src="shared.js"></script>
  <script>LD.mountChrome('home');</script>
</body>
```

Toda sección lleva `.reveal` para el IntersectionObserver (fade-in + translateY 14px → 0). Se respeta `prefers-reduced-motion`.

---

## Components

Organizados por uso. Todos viven en `shared.css`.

### Masthead (`shared.js` → `renderMasthead(active)`)

Sticky top, backdrop blur, `border-bottom: 1px solid var(--rule-strong)`.

- **Brand**: `La Datáfora` + `VOL. 01` (mono, `--ink-mute`).
- **Nav**: 5 enlaces. Activo tiene underline de 2px (`::after` posicionado `bottom: -15px`).
- **Meta derecha**: dot pulsante (`--signal`, animación 2.6s) + `LIMA · DD MES YYYY`.
- **Issue strip** (segunda fila): nombre del autor / centro editorial / locale.
- **Mobile ≤780px**: 2 filas, nav se vuelve scroll horizontal, meta se oculta.

**Prop**: `active` acepta `'home' | 'cv' | 'libro' | 'incorer' | 'prosecnur'`.

### Footer (`shared.js` → `renderFooter()`)

- Grid 4 cols: Descripción · Navegar · Producido en · Contacto.
- Big typographic mark serif (12vw) como firma visual.
- Colophon con año y stack tipográfico.
- Mobile: 2 cols.

### Botones

```html
<a class="btn">Texto <span class="arrow">→</span></a>
<a class="btn btn-ghost">Texto</a>
```

| Clase | Default | Hover |
|---|---|---|
| `.btn` | Fondo `--ink`, texto `--paper`, borde `--ink`, padding `12px 18px` | Fondo transparente, texto `--ink`, `translateY(-1px)` |
| `.btn.btn-ghost` | Transparente, texto `--ink` | Invierte a sólido |
| `.btn .arrow` | — | `translateX(3px)` en hover del botón |
| `.btn` dentro de `.install` / `.contact` | Papel invertido sobre fondo oscuro | Transparente con borde papel |

**Border-radius**: `0` (intencional — estilo editorial, no pill).
**Transition**: `background-color .2s, color .2s, transform .15s`.

### Chip

```html
<span class="chip">R</span>
<span class="chip chip-fill">Destacado</span>
```

Pill `border-radius: 999px`, borde `--rule-strong`, mono `.72rem`. `.chip-fill` invierte a ink.

### Section head

```html
<div class="section-head">
  <div class="num-label">§ 01 / Categoría</div>
  <div>
    <h2>Título <em>cursivo</em></h2>
    <p class="lede">Texto de bajada editorial.</p>
  </div>
</div>
```

Grid `88px 1fr`, `border-top: 1px solid var(--rule-strong)`, padding vertical fluido. El `<h2>` es serif italic — **no usar font-weight bold** en ningún h2.

### Hero (Home)

`.hero` + `.hero-grid` (`1fr 360px`). Lado izq: kicker mono (uppercase con border-top ink), `h1.hero-title` serif con `em` cursivos + `.amp` acento, `.hero-lede`. Lado der `.hero-side`: `.hero-numbers` grid 2×2 de métricas con `<span class="big">` serif italic 2rem, más `.hero-spark` sparkline SVG.

Variantes controladas por `data-hero` attr (opcionales — el sitio actual usa el default "data"):
- `data-hero="type"` → título gigante (14rem), oculta sidebar
- `data-hero="data"` → defecto, grid numérico
- `data-hero="image"` → placeholder retrato

### Despachos (featured cards)

`.dispatches-grid` grid `1.4fr 1fr 1fr`. Cada `.dispatch` tiene:
- `.d-kicker` (top, mono uppercase)
- `<h3>` serif con `em` terracotta
- `.d-lead` párrafo
- `.d-chart` SVG mini-chart (56px alto)
- `.d-foot` (bottom, border-top `--rule`)

`.dispatch.lead` escala el h3 de 2rem a 3rem. En hover, stroke-width de sparklines aumenta y dot crece.

### Corpus list

`<ul class="corpus-list">` con `<li class="corpus-row">` grid `40px 2.5fr 1.2fr 1fr 90px`.

Columnas: idx mono · título serif con `em` + `.sub` sans · meta mono · bar SVG (28px) · go label.

Hover: `padding-left: 8px` (slide), fondo `--paper-soft`, barras cambian fill a `--accent`.

En mobile (`≤900px`), queda `30px 1fr auto` — se ocultan meta y bar.

### Signals (quote + stats)

`.signals` grid `1.4fr 1fr`. Blockquote serif italic XL con glyph `"` terracotta. `.signals-stats` grid 2×2 con borders, cada `.stat` hover a `--paper-soft`.

### Libro block

`.libro-wrap` grid `380px 1fr`:
- `.libro-cover` aspect-ratio 3/4, `box-shadow: 6px 6px 0 var(--accent)` (offset terracotta), hover `translate(-2px,-2px)` con shadow a `10px 10px 0`.
- `.libro-text` con toc `.libro-toc` grid 3 cols, cada `.ch` auto-numerado (CSS counter).

### Chapters (libro.html detail)

`.chapters` + `.chapter` grid `110px 1fr`. Numeración mono a la izquierda, h3 serif.

### Now (current work)

`.now` grid 3 cols, cada `.col` con `.k` (mono uppercase), `.v` (serif italic 1.4rem), `.t` (mono timestamp).

### CV Hero

`.cv-hero` grid `1fr 380px`. Nombre `.cv-name` serif 6.2rem responsive, role/lede, card lateral `.cv-card` con filas `96px 1fr` mono.

### Timeline (.tl)

```html
<article class="tl">
  <div class="tl-when">abr 2026 — actualidad</div>
  <div>
    <h3 class="tl-role"><em>Analista</em> de proyectos</h3>
    <div class="tl-org">PULSO PUCP · Lima</div>
    <p class="tl-body">…</p>
    <div class="tl-chips"><span class="chip">R</span>…</div>
  </div>
</article>
```

Grid `140px 1fr`. Hover → fondo paper-soft. `:last-of-type` gana border-bottom.

### Products (.prods)

Grid 3 cols con hover. Cada `.prod` tiene kicker, h3 serif 2rem, body corto, `.p-chart` (scatter/bars/sparkline 110px), `.p-foot` con link subrayado.

### Cases (.cases)

`.case` grid `120px 1fr 320px`: índice mono / contenido / thumb. `.case .thumb` por default es patrón de líneas diagonales (`repeating-linear-gradient`); con `.thumb.has-img` acepta una imagen (`object-fit: cover`). `.case:hover` desliza `padding-left: 10px`.

### Contact (dark block)

`.contact` fondo `--ink`, texto `--paper`, grid `1.2fr 1fr`. H2 serif gigante con em `--accent-soft`. `.btn` invertido a papel. `.contact-side` rows punteadas.

### Package hero (.ph)

Igual patrón que CV hero pero con `.ph-title` serif 7.2rem, `.ph-sub` cursivo y `.spec` aside con filas mono. La primera `.spec .row.ver` es invertida (fondo ink).

### Schematic flow (.flow)

Grid 5 cols con nodos numerados. Cada `.flow .node` tiene `.n` (top-left mono), `.tick` (círculo 24px top-right), h4 serif, párrafo. Hover → tick cambia a ink sobre paper-soft.

Mobile ≤900px: grid 2 cols.

### Features (2×2)

`.features` grid 2 cols. `.feat` min-height 260px con número serif italic 2.4rem acento, h3 serif 1.7rem, body. Borders intercalados (nth-child). Mobile: 1 col.

### Code-wrap (explain + code side-by-side)

`.code-wrap` grid 2 cols. Izq: `.code-side` con num-label, h3, párrafo y lista mono. Der: `.code-block` con `.ck-head` (dot signal + file + tag) y `<pre>` syntax-highlighted con spans `.com .kw .fn .str`. Sobre fondo `--ink`.

### Rules list (.rules-list)

`<ul>` con rows grid `60px 2fr 1.5fr auto`. Pill `.st` con variantes `.ok` (signal), `.warn` (accent), `.err` (ink fill).

### Report grid (Prosecnur specific)

Grid 24 cols generado por JS. Celdas `.ok .warn .err .miss` con fondos respectivos + hover scale(1.3). Debajo `.report-summary` con 4 stats.

### Install block

`.install` fondo `--ink`, grid 2 cols, incluye `.code-block` con fondo aún más oscuro (`#0a0a0a`). Usado al final de landings de paquete.

### FAQ

`<details>` nativos estilizados. Summary serif 1.5rem con `em` italic acento. `::after` muestra `+` / `–` (no el triángulo nativo).

### Datáfora Interactiva block (.dint)

Bloque editorial al final de `libro.html`: fondo `--paper-deep`, grid `1.2fr 1fr`. Izq título serif XL + párrafos + CTA. Der `.dint-side` lista mono con dashes.

### Long-form document (.doc + .doc-hero)

Usado por `manual-incorer.html` y `guia-prosecnur.html`.

- `.doc-hero`: kick mono, h1 serif XL, subtitle italic, meta row con fields `<strong>`.
- `.doc`: contenedor `max-width: 780px` centrado, estilo tipográfico de lectura larga.
  - h1/h2/h3/h4 serif (excepto h4-h6 que son sans bold para UI).
  - `.header-section-number` mono pequeño antes de cada heading numerado.
  - Inline `<code>` con fondo `--paper-deep`.
  - Bloques `<pre>` y `.sourceCode` → fondo `--ink`, texto papel, tokens coloreados (`.kw .st .fu .co .op .dv`).
  - Figuras → borde rule-strong, caption mono muted.
  - Tablas → thead con border-bottom ink, hover de fila.
  - `.callout` Quarto con border-left accent.
- `.doc-layout` opcional para añadir TOC sticky a la derecha en ≥1100px.

---

## States & Interactions

| Elemento | Estado | Comportamiento |
|---|---|---|
| `.btn` | Hover | Inversión color + `translateY(-1px)`, flecha `translateX(3px)` |
| `.btn` | Focus | Recibe outline nativo (no overrideado) — keyboard friendly |
| `.mast-nav a.active` | — | Underline 2px ink |
| `.reveal` | In viewport | `opacity 0→1 + translateY 14px→0` (.7s easing) |
| `.corpus-row` | Hover | `padding-left: 8px`, fondo paper-soft, barras → accent |
| `.tl` | Hover | Fondo paper-soft |
| `.prod` | Hover | Paper-soft + scatter circles terracotta en 1/3 |
| `.dispatch` | Hover | Paper-soft, sparkline stroke-width 1.8, dot r 4.2 |
| `.case` | Hover | `padding-left: 10px`, fondo paper-soft |
| `.libro-cover` | Hover | `translate(-2px,-2px)` con shadow terracotta más grande |
| `.flow .node` | Hover | Paper-soft + tick invertido a accent |
| `.feat` | Hover | Paper-soft |
| `details` | Open | Fondo paper-soft, summary marker cambia `+` → `–` |
| `.report-grid .cell` | Hover | `scale(1.3)`, z-index 2, border ink |

Animaciones respetan `prefers-reduced-motion: reduce` (desactivan todos los reveals).

---

## Responsive Behavior (resumen)

| Breakpoint | Cambios principales |
|---|---|
| **Desktop ≥1100px** | Layouts completos, TOC flotante en manuales |
| **Tablet 901-1099** | Grid gutters se achican |
| **Tablet 781-900** | Heroes `1fr`, grids `.prods .features .dispatches .flow .contact .install .signals` → 1-2 cols |
| **Mobile ≤780** | Masthead 2-line, nav scroll-x, meta oculto; footer 2 cols |
| **Mobile ≤700** | `.tl` pierde columna when; `.case` pasa `60px 1fr` con thumb full-width |

---

## Accessibility

- **Skip/focus**: botones y links usan outline nativo. Orden DOM = orden visual (no flexbox reorder visual-only).
- **Headings**: cada página tiene exactamente un `<h1>`. Los `section` usan `aria-labelledby` apuntando al id del heading.
- **Masthead**: `<header>` con `<nav class="mast-nav">`. Dot `.signal` decorativo → `aria-hidden="true"` implícito por contenido vacío.
- **Sections**: los `.reveal` arrancan con `opacity:0`; los usuarios con `prefers-reduced-motion` o JS desactivado **ven todo** igualmente (la regla `@media (prefers-reduced-motion)` fuerza visible; sin JS falla abierto porque IntersectionObserver nunca agrega la clase — considerar fallback: añadir `.is-visible` por JS en pageload timeout como red de seguridad).
- **Figuras**: todas las imágenes decorativas llevan `alt=""`; las de contenido tienen alt descriptivo.
- **Contrast**: `--ink` sobre `--paper` supera AAA para texto normal. `--ink-mute` sobre `--paper` pasa AA para ≥1rem.
- **Code blocks**: colores sobre `--ink` testeados — tokens `.str` (#cbd6a0), `.kw` (accent-soft), `.fn` (paper) mantienen ≥4.5:1.

**Pendientes conocidos**:
- Agregar `<main>` ARIA role explícito.
- `.corpus-row[onclick]` usa click handler en `<li>` — para keyboard accessibility debería convertirse en `<a>` envolvente o añadir `tabindex="0"` + key handler.
- FAQ `<details>` ya es keyboard accesible nativamente.

---

## JS Helpers (`shared.js`)

Namespace global `window.LD`:

| Método | Uso |
|---|---|
| `LD.mountChrome(active)` | Monta masthead + footer, dispara reveal observer |
| `LD.renderMasthead(active)` | Devuelve HTML string del masthead |
| `LD.renderFooter()` | HTML string del footer |
| `LD.wireReveal()` | IntersectionObserver para `.reveal` |
| `LD.sparkline(values, {width, height, stroke})` | SVG polyline; valores en [0,1] |
| `LD.bars(values, {width, height, fill})` | SVG barchart |
| `LD.scatter(points, {width, height})` | SVG scatter; points `{x, y, r, o}` todos en [0,1] |

Todo es vanilla, ≈5KB, sin deps. Inyectar en cada página:

```html
<script src="shared.js"></script>
<script>LD.mountChrome('home');</script>
```

---

## Edge Cases & Content Rules

| Caso | Regla |
|---|---|
| **Hero title** | Máx 3 líneas cortas (home) o 2 (paquetes). Usar `<br>` manuales para ritmo. |
| **Lede / `.hero-lede`** | `max-width: 46ch` — evita párrafos demasiado anchos. |
| **Corpus row title** | Si el título excede el ancho, hacer 2 líneas en lugar de truncar. |
| **Timeline body** | `max-width: 68ch`. Largos de 4-6 líneas son ideales. |
| **Cards scatter chart** | Valores en `[0,1]`; NO normalizar en render. |
| **Figure without image** | `.case .thumb` sin `.has-img` muestra patrón diagonal como placeholder. |
| **Empty state (índice de corpus vacío)** | No hay vista diseñada — por ahora el sitio siempre tiene data. Si aplica, mostrar `.section-head` con lede "Próximamente" y sección sin ul. |
| **Loading** | No hay estados de carga (sitio estático). Fonts hacen FOUT con fallback Georgia/Inter. |
| **Error** | No aplica — páginas estáticas renderizadas. |
| **Texto largo internacional** | Serif clamp y sans clamp manejan +30% bien. Probado en ES-PE. |
| **Conexiones lentas** | Fuentes con `display: swap`. Imágenes sin lazy-loading explícito — añadir `loading="lazy"` a figuras no críticas del manual si vuelve a editarse. |

---

## Animation / Motion

| Elemento | Trigger | Animación | Duración | Easing |
|---|---|---|---|---|
| `.reveal` | IntersectionObserver threshold .08 | opacity + translateY 14px | 700ms | cubic-bezier(.2,.8,.2,1) |
| `.mast-meta .dot` | Load | Pulse (opacity 1 → .35 → 1) | 2.6s | ease-in-out loop |
| `.btn` | Hover | transform + color | 150-200ms | default |
| `.btn .arrow` | Hover btn | translateX 3px | 250ms | ease |
| `.libro-cover` | Hover | translate + shadow grow | 400ms | ease |
| `.dispatch:hover .sparkline polyline` | Hover | stroke-width 1.25 → 1.8 | 200ms | default |
| `.dispatch:hover .sparkline-dot` | Hover | r 3 → 4.2 | 200ms | ease |
| `.flow .node` | Hover | tick invert | 250ms | default |
| `.report-grid .cell` | Hover | scale(1.3) | 150ms | default |
| `.tl` | Hover | background-color fade | 200ms | default |

Reduce-motion: desactiva `.reveal` transition. Mantener hovers sutiles (≤200ms transform).

---

## Conventions & Guardrails

1. **Nunca literales de color/espacio**: usar siempre `var(--token)`. Si falta un token, agrégalo a `:root`.
2. **Cursivas terracota**: reservadas para énfasis editorial dentro de títulos serif. No usar en párrafos.
3. **Mayúsculas mono**: para kickers, labels, metadatos. Nunca para títulos.
4. **Border-radius 0**: intencional. Botones, cards, chips (excepto chip 999px) son rectángulos.
5. **Weight 400**: default en serif. Bold en serif rompe el tono editorial.
6. **Ink block inserts**: `.contact` y `.install` son los únicos bloques de fondo oscuro full-width. Sus botones invierten a papel.
7. **Masthead nav**: siempre las mismas 5 entradas (portada, cv, libro, incorer, prosecnur). Los manuales se alcanzan desde sus landings, no desde el top nav.

---

## Deployment & Dev

### Local dev

```bash
cd docs
python3 -m http.server 8765
# abrir http://localhost:8765/
```

No hay build — editar, guardar, refresh.

### Deploy

GitHub Pages apunta a `docs/` de `main`. Push → publica. No hay workflow CI.

### Adding a new page

1. Crear `docs/nueva.html` copiando `incorer.html` como plantilla.
2. Agregar la entrada en `PAGES` dentro de `shared.js` con `href`, `label`, `key`.
3. También enlazarla en el `<h4>Navegar</h4>` del footer dentro de `renderFooter()`.
4. Invocar `LD.mountChrome('key')` al final de la página.

---

## Referencia rápida de tokens

```css
/* Solo los más usados */
background: var(--paper);
color: var(--ink);
color: var(--ink-soft);        /* cuerpo */
color: var(--ink-mute);        /* meta */
border-top: 1px solid var(--rule-strong);
font-family: var(--serif);     /* títulos */
font-family: var(--mono);      /* labels */
font-size: var(--step-3);      /* h2 */
padding-inline: var(--pad-inline);
```
