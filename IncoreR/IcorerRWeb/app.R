# app.R — INCOREr + La Datafora (modular, móvil & retina)
suppressPackageStartupMessages({
  library(shiny)
  library(incorer)
  library(ragg)
  library(DT)
  library(bslib)
})

options(timeout = 180)

# ───────────────────────────────────────────────────────────────────────────────
# UTILIDADES COMPARTIDAS
# ───────────────────────────────────────────────────────────────────────────────

normalize_pilar <- function(x) {
  if (is.null(x)) return(NULL)
  if (identical(x, "__GENERAL__")) return(NULL)
  if (toupper(x) %in% c("GEN","GENERAL")) return(NULL)
  x
}

normalize_regiones <- function(x) {
  if (is.null(x) || length(x) == 0) return("ALL")
  if ("ALL" %in% x) return("ALL")
  unique(x)
}

build_script_general_barras <- function(edicion, pilar, regiones, incluir_peru, paleta, mostrar_leyenda) {
  regiones_txt <- if (identical(regiones, "ALL")) '"ALL"' else {
    paste0("c(", paste0('"', regiones, '"', collapse = ", "), ")")
  }
  pilar_txt <- if (is.null(pilar)) "NULL" else paste0('"', pilar, '"')
  paste0(
    "library(incorer)\n",
    "edicion <- ", edicion, "\n",
    "pilar <- ", pilar_txt, "\n",
    "regiones <- ", regiones_txt, "\n",
    "g <- general_barras(edicion, pilar, regiones,\n",
    "  usar_codigos = TRUE, incluir_peru = ", isTRUE(incluir_peru),
    ", paleta = \"", paleta, "\", mostrar_leyenda = ", isTRUE(mostrar_leyenda), ")\n",
    "print(g)\n"
  )
}

# Código reproducible para general_tabla()
build_script_general_tabla <- function(edicion, pilar, regiones, incluir_peru){
  regiones_txt <- if (identical(regiones, "ALL")) '"ALL"' else {
    paste0("c(", paste0('"', regiones, '"', collapse = ", "), ")")
  }
  pilar_txt <- if (is.null(pilar)) "NULL" else paste0('"', pilar, '"')
  paste0(
    "library(incorer)\n",
    "edicion <- ", edicion, "\n",
    "pilar <- ", pilar_txt, "\n",
    "regiones <- ", regiones_txt, "\n",
    "tab <- general_tabla(edicion, pilar, regiones,\n",
    "  usar_codigos = TRUE, incluir_peru = ", isTRUE(incluir_peru), ")\n",
    "tab\n"
  )
}

# Código reproducible: general_dispersion_pilares()
build_script_general_dispersion <- function(
    edicion, regiones, pilares, paleta,
    mostrar_promedio, promedio_shape, promedio_size,
    promedio_color, promedio_fill, jitter_width, jitter_height
){
  reg_txt <- if (identical(regiones, "ALL")) '"ALL"' else {
    paste0("c(", paste0('"', regiones, '"', collapse = ", "), ")")
  }
  pil_txt <- if (identical(pilares, "ALL")) '"ALL"' else {
    paste0("c(", paste0('"', pilares, '"', collapse = ", "), ")")
  }
  paste0(
    "library(incorer)\n",
    "edicion <- ", edicion, "\n",
    "regiones <- ", reg_txt, "\n",
    "pilares  <- ", pil_txt, "\n",
    "g <- general_dispersion_pilares(\n",
    "  edicion = edicion,\n",
    "  regiones = regiones,\n",
    "  pilares = pilares,\n",
    "  usar_codigos = TRUE,\n",
    "  paleta = \"", paleta, "\",\n",
    "  mostrar_promedio = ", isTRUE(mostrar_promedio), ",\n",
    "  promedio_shape = ", as.integer(promedio_shape), ",\n",
    "  promedio_size = ", as.numeric(promedio_size), ",\n",
    "  promedio_color = \"", promedio_color, "\",\n",
    "  promedio_fill  = \"", promedio_fill, "\",\n",
    "  jitter_width  = ", as.numeric(jitter_width), ",\n",
    "  jitter_height = ", as.numeric(jitter_height), "\n",
    ")\n",
    "print(g)\n"
  )
}

# JS util para copiar
copy_js <- HTML("
  function copyScriptById(id){
    try{
      var el = document.getElementById(id);
      var txt = el ? el.innerText : '';
      if(navigator.clipboard && navigator.clipboard.writeText){
        navigator.clipboard.writeText(txt);
      }else{
        var ta = document.createElement('textarea');
        ta.value = txt; document.body.appendChild(ta); ta.select();
        document.execCommand('copy'); document.body.removeChild(ta);
      }
    }catch(e){ console.warn('No se pudo copiar', e); }
  }
")

# JS para enviar DPR (device pixel ratio) al server y refrescar en resize
dpr_js <- HTML("
  function sendDPR(){
    Shiny.setInputValue('.__dpr__', window.devicePixelRatio || 1, {priority:'event'});
  }
  window.addEventListener('load',  sendDPR);
  window.addEventListener('resize', sendDPR);
")

# CSS extra (responsive + navbar + gráficos fluidos)
css_extras <- HTML("
  /* Fondo y tipografía de seguridad */
  html, body, .container-fluid, .tab-content {
    background: var(--bg) !important;
    color: var(--text) !important;
  }
  h1,h2,h3,h4,h5 { color: var(--accent) !important; }

  /* Navbar refinada (título y tabs marrones) */
  .navbar, .navbar-light, .navbar-dark{
    background: var(--bg-soft) !important;
    border-bottom: 1px solid var(--border) !important;
  }
  .navbar-brand { color: var(--accent) !important; font-weight: 600 !important; }
  .navbar-nav > li > a, .navbar-nav .nav-link, .navbar-nav .dropdown-toggle {
    color: var(--accent) !important; font-weight: 500; transition: .2s;
  }
  .navbar-nav .nav-link:hover, .navbar-nav .nav-link:focus, .navbar-nav .nav-link.active {
    background-color: var(--bg-soft) !important; color: var(--accent) !important; border-radius: 6px;
  }
  .dropdown-menu{
    background-color: var(--bg-soft) !important; border:1px solid var(--border);
    border-radius:10px; box-shadow:0 3px 10px rgba(0,0,0,.08);
  }
  .dropdown-menu .dropdown-item{ color: var(--text) !important; font-weight:500; }
  .dropdown-menu .dropdown-item:hover, .dropdown-menu .dropdown-item:focus{
    background-color: rgba(199,164,122,.25) !important; color: var(--accent) !important;
  }

  /* Sidebar y tarjeta (por si el CSS externo no pisa todo) */
  .sticky-sidebar{
    position: sticky; top: 20px;
    background: var(--bg-soft);
    border: 1px solid var(--border);
    border-radius: 12px; padding: 14px;
  }
  .result-card{
    background: #fff; border: 1px solid var(--border);
    border-radius: 12px; padding: 0; box-shadow: 0 2px 8px rgba(0,0,0,.06);
    overflow: visible;
  }

  /* En R */
  .code-block{
    background:#f1eee9; border:1px solid var(--border);
    border-radius:8px; padding:1rem; color:var(--text);
    max-height:240px; overflow:auto;
  }
  .btn-center{ display:flex; justify-content:center; margin:.25rem 0; }

  /* Plot responsive */
  .ldf-plot{ width:100%; height:auto; display:flex; justify-content:center; align-items:center; }
  .ldf-plot img{ width:100% !important; height:auto !important; display:block !important; margin:0 !important; }

  /* Mobile: apilar y espaciar */
  @media (max-width: 768px){
    .sticky-sidebar{ margin-bottom: 12px; }
    .navbar-brand{ font-size: 1.1rem; }
    .code-block{ max-height: 180px; }
  }
")


# ── helpers comunes ────────────────────────────────────────────────────────────
normalize_pilar <- function(x) {
  if (is.null(x)) return(NULL)
  if (identical(x, "__GENERAL__")) return(NULL)
  if (toupper(x) %in% c("GEN","GENERAL")) return(NULL)
  x
}
normalize_regiones <- function(x) {
  if (is.null(x) || length(x) == 0) return("ALL")
  if ("ALL" %in% x) return("ALL")
  unique(x)
}

# Render PNG retina en pantalla (usa ancho CSS y oversampling)
.render_png_resp <- function(pngfile, width_css, dpr = 1, plot_fun){
  safe_num1 <- function(x, default){
    x <- suppressWarnings(as.numeric(x))
    if (length(x)!=1 || is.na(x) || !is.finite(x)) return(default)
    x
  }
  w_css <- safe_num1(width_css, 1000); if (w_css <= 0) w_css <- 1000
  # mínimo 2× para nitidez
  oversample <- if (isTRUE(dpr >= 1)) max(dpr, 2) else 2
  w_px <- as.integer(round(w_css * oversample)); if (w_px <= 0) w_px <- 2000
  h_px <- as.integer(round(w_px * 0.62));        if (h_px <= 0) h_px <- 1240
  res_ <- 96 * oversample
  
  ragg::agg_png(pngfile, width = w_px, height = h_px, units = "px", res = res_, background = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  plot_fun()
}

# Guardar PNG alta resolución para descarga (fijo y nítido)
.save_png_hq <- function(file, plot_fun, width_px = 3000, height_px = 1860, res = 300){
  ragg::agg_png(file, width = width_px, height = height_px, units = "px", res = res, background = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  plot_fun()
}

# ── operador "o si no" ───────────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && (is.na(x) || !is.finite(suppressWarnings(as.numeric(x)))))) y else x
}

# ─────────────────────────────────────────────────────────────
# Placeholders seguros (evitan if/else inline dentro de p())
# ─────────────────────────────────────────────────────────────
placeholder_ui <- function(id, titulo, descripcion = NULL) {
  ns <- NS(id)
  txt <- if (is.null(descripcion)) "Este módulo se activará pronto." else descripcion
  tagList(
    fluidRow(
      column(
        width = 4,
        div(class = "sticky-sidebar",
            h4("Parámetros"),
            p(class = "muted", txt)
        )
      ),
      column(
        width = 8,
        div(class = "result-card",
            div(style = "padding:14px;",
                h4(titulo),
                p("Los módulos están aislados: un fallo aquí no afecta al resto.")
            )
        )
      )
    )
  )
}

placeholder_server <- function(id) {
  moduleServer(id, function(input, output, session){ })
}

# ───────────────────────────────────────────────────────────────────────────────
# MÓDULO: GENERAL ▸ BARRAS
# ───────────────────────────────────────────────────────────────────────────────

general_barras_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        div(class = "sticky-sidebar",
            h4("Parámetros"),
            selectInput(ns("edicion"), "Edición (año)",
                        choices = 2016:2025, selected = 2025),
            selectInput(ns("pilar"), "Pilar",
                        choices = c("General" = "__GENERAL__"),
                        selected = "__GENERAL__"),
            selectizeInput(ns("regiones"), "Regiones",
                           choices = c("Todas (ALL)" = "ALL"),
                           selected = "ALL", multiple = TRUE,
                           options = list(
                             plugins = list("remove_button"),
                             placeholder = "Selecciona regiones o ALL"
                           )),
            checkboxInput(ns("incluir_peru"), "Incluir \"Perú\"", value = FALSE),
            selectInput(ns("paleta"), "Paleta",
                        choices = c("ipe", "okabe_ito", "viridis"), selected = "ipe"),
            checkboxInput(ns("mostrar_leyenda"), "Mostrar leyenda", value = FALSE),
            
            div(class = "btn-center-vertical",
                actionButton(ns("go"), "Generar gráfico", class = "btn btn-primary"),
                downloadButton(ns("dl_png"), " Descargar PNG", class = "btn btn-primary")
            )
        )
      ),
      column(
        width = 8,
        div(class = "result-card",
            div(class = "ldf-plot",
                imageOutput(ns("img"), height = "auto")
            )
        )
      )
    )
  )
}

general_barras_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Catálogos
    cat_p <- tryCatch(incorer::catalogo_pilar(), error = function(e) NULL)
    pilar_choices <- c("General"="__GENERAL__")
    if (!is.null(cat_p)) {
      if (all(c("codigo","nombre") %in% names(cat_p))) {
        pilar_choices <- c(pilar_choices, stats::setNames(cat_p$codigo, cat_p$nombre))
      } else if (all(c("pilar","pilar_nombre") %in% names(cat_p))) {
        pilar_choices <- c(pilar_choices, stats::setNames(cat_p$pilar, cat_p$pilar_nombre))
      }
    }
    updateSelectInput(session, "pilar", choices = pilar_choices)
    
    cat_r <- tryCatch(incorer::catalogo_region(), error = function(e) NULL)
    reg_choices <- c("Todas (ALL)"="ALL")
    if (!is.null(cat_r)) {
      if (all(c("codigo","nombre") %in% names(cat_r))) {
        reg_choices <- c(reg_choices, stats::setNames(cat_r$codigo, cat_r$nombre))
      } else if (all(c("region","region_nombre") %in% names(cat_r))) {
        reg_choices <- c(reg_choices, stats::setNames(cat_r$region, cat_r$region_nombre))
      }
    }
    updateSelectizeInput(session, "regiones", choices = reg_choices, selected = "ALL")
    
    observeEvent(input$go, {
      output$img <- renderImage({
        pngfile <- tempfile(fileext = ".png")
        key_ns <- paste0("output_", session$ns("img"), "_width")
        w_css <- session$clientData[[key_ns]] %||% 1000
        dpr <- input$.__dpr__ %||% 1
        .render_png_resp(
          pngfile, width_css = w_css, dpr = dpr,
          plot_fun = function() {
            g <- incorer::general_barras(
              edicion         = as.integer(input$edicion),
              pilar           = normalize_pilar(input$pilar),
              regiones        = normalize_regiones(input$regiones),
              usar_codigos    = TRUE,
              incluir_peru    = isTRUE(input$incluir_peru),
              paleta          = input$paleta,
              mostrar_leyenda = isTRUE(input$mostrar_leyenda)
            )
            if (inherits(g, "ggplot")) print(g) else grid::grid.draw(g)
          }
        )
        list(src = pngfile, contentType = "image/png", width = "100%",
             deleteFile = TRUE)
      }, deleteFile = TRUE)
    })
    
    output$dl_png <- downloadHandler(
      filename = function() paste0("general_barras_", input$edicion, ".png"),
      content = function(file) {
        .save_png_hq(file, plot_fun = function() {
          g <- incorer::general_barras(
            edicion         = as.integer(input$edicion),
            pilar           = normalize_pilar(input$pilar),
            regiones        = normalize_regiones(input$regiones),
            usar_codigos    = TRUE,
            incluir_peru    = isTRUE(input$incluir_peru),
            paleta          = input$paleta,
            mostrar_leyenda = isTRUE(input$mostrar_leyenda)
          )
          if (inherits(g, "ggplot")) print(g) else grid::grid.draw(g)
        })
      }
    )
  })
}


# ─────────────────────────────────────────────────────────────
# UI: GENERAL ▸ DISPERSIÓN POR PILAR (acordeón visual)
# ─────────────────────────────────────────────────────────────
general_dispersion_pilares_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        div(class = "sticky-sidebar",
            h4("Parámetros"),
            selectInput(ns("edicion"), "Edición (año)", choices = 2016:2025, selected = 2025),
            
            # Pilares (multi) con ALL exclusivo
            selectizeInput(ns("pilares"), "Pilares",
                           choices  = c("Todos (ALL)" = "ALL"),
                           selected = "ALL", multiple = TRUE,
                           options  = list(plugins = list("remove_button"),
                                           placeholder = "Selecciona pilares o ALL")),
            
            # Regiones (multi) con ALL exclusivo
            selectizeInput(ns("regiones"), "Regiones",
                           choices  = c("Todas (ALL)" = "ALL"),
                           selected = "ALL", multiple = TRUE,
                           options  = list(plugins = list("remove_button"),
                                           placeholder = "Selecciona regiones o ALL")),
            
            selectInput(ns("paleta"), "Paleta", choices = c("ipe","okabe_ito","viridis"), selected = "ipe"),
            checkboxInput(ns("mostrar_promedio"), "Mostrar promedio nacional", value = TRUE),
            
            # Acordeón visual para opciones avanzadas
            bslib::accordion(
              id = ns("adv_opts"),
              bslib::accordion_panel(
                title = "Opciones adicionales",
                div(style="margin-top:.25rem;",
                    selectInput(ns("promedio_shape"), "Forma promedio", choices = c(21,22,23,24,25), selected = 23),
                    numericInput(ns("promedio_size"),  "Tamaño promedio", value = 3.5, min = 1, max = 10, step = .5),
                    div(class="row",
                        div(class="col-sm-6",
                            textInput(ns("promedio_color"), "Color borde promedio", value = "black")),
                        div(class="col-sm-6",
                            textInput(ns("promedio_fill"),  "Relleno promedio",    value = "white"))
                    ),
                    sliderInput(ns("jitter_width"),  "Jitter ancho",  min = 0, max = 0.3, value = 0.08, step = 0.01),
                    sliderInput(ns("jitter_height"), "Jitter alto",   min = 0, max = 0.3, value = 0,    step = 0.01)
                ),
                open = FALSE
              )
            ),
            
            div(class = "btn-center",
                actionButton(ns("go"), "Generar gráfico", class = "btn btn-primary")
            ),
            
            tags$hr(),
            h5("En R"),
            div(style="display:flex; gap:.5rem; align-items:center; margin-bottom:.5rem;",
                actionLink(ns("copy_script"), label = "Copiar", icon = icon("clipboard"))
            ),
            pre(id = ns("script"), class = "code-block")
        )
      ),
      column(
        width = 8,
        div(class = "result-card",
            div(class = "ldf-plot",
                imageOutput(ns("img"), height = "auto")
            )
        )
      )
    )
  )
}

# ─────────────────────────────────────────────────────────────
# SERVER: GENERAL ▸ DISPERSIÓN POR PILAR  
# ─────────────────────────────────────────────────────────────

general_dispersion_pilares_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        div(class="sticky-sidebar",
            h4("Parámetros"),
            selectInput(ns("edicion"), "Edición (año)",
                        choices=2016:2025, selected=2025),
            selectizeInput(ns("pilares"), "Pilares",
                           choices=c("Todos (ALL)"="ALL"),
                           selected="ALL", multiple=TRUE,
                           options=list(plugins=list("remove_button"),
                                        placeholder="Selecciona pilares o ALL")),
            selectizeInput(ns("regiones"), "Regiones",
                           choices=c("Todas (ALL)"="ALL"),
                           selected="ALL", multiple=TRUE,
                           options=list(plugins=list("remove_button"),
                                        placeholder="Selecciona regiones o ALL")),
            selectInput(ns("paleta"), "Paleta",
                        choices=c("ipe","okabe_ito","viridis"), selected="ipe"),
            checkboxInput(ns("mostrar_promedio"), "Mostrar promedio nacional", TRUE),
            
            bslib::accordion(
              id=ns("adv_opts"),
              bslib::accordion_panel(
                "Opciones adicionales",
                selectInput(ns("promedio_shape"), "Forma promedio", choices=c(21,22,23,24,25), selected=23),
                numericInput(ns("promedio_size"), "Tamaño promedio", 3.5, min=1, max=10, step=0.5),
                textInput(ns("promedio_color"), "Color borde", "black"),
                textInput(ns("promedio_fill"), "Relleno", "white"),
                sliderInput(ns("jitter_width"), "Jitter ancho", 0, 0.3, 0.08, step=0.01),
                sliderInput(ns("jitter_height"), "Jitter alto", 0, 0.3, 0, step=0.01)
              )
            ),
            
            div(class="btn-center-vertical",
                actionButton(ns("go"), "Generar gráfico", class="btn btn-primary"),
                downloadButton(ns("dl_png"), "Descargar PNG", class="btn btn-primary")
            )
        )
      ),
      column(
        width = 8,
        div(class="result-card",
            div(class="ldf-plot",
                imageOutput(ns("img"), height="auto")
            )
        )
      )
    )
  )
}

general_dispersion_pilares_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    cat_p <- tryCatch(incorer::catalogo_pilar(), error=function(e)NULL)
    pil_choices <- c("Todos (ALL)"="ALL")
    if(!is.null(cat_p)){
      if(all(c("codigo","nombre") %in% names(cat_p))){
        cat_p <- subset(cat_p, toupper(codigo)!="GEN")
        pil_choices <- c(pil_choices, stats::setNames(cat_p$codigo, cat_p$nombre))
      }
    }
    updateSelectizeInput(session,"pilares",choices=pil_choices,selected="ALL")
    
    cat_r <- tryCatch(incorer::catalogo_region(), error=function(e)NULL)
    reg_choices <- c("Todas (ALL)"="ALL")
    if(!is.null(cat_r)){
      if(all(c("codigo","nombre") %in% names(cat_r))){
        reg_choices <- c(reg_choices, stats::setNames(cat_r$codigo, cat_r$nombre))
      }
    }
    updateSelectizeInput(session,"regiones",choices=reg_choices,selected="ALL")
    
    observeEvent(input$go,{
      output$img <- renderImage({
        pngfile <- tempfile(fileext=".png")
        key_ns <- paste0("output_", session$ns("img"), "_width")
        w_css <- session$clientData[[key_ns]] %||% 1000
        dpr <- input$.__dpr__ %||% 1
        .render_png_resp(
          pngfile, width_css=w_css, dpr=dpr,
          plot_fun=function(){
            g <- incorer::general_dispersion_pilares(
              edicion          = as.integer(input$edicion),
              regiones         = normalize_regiones(input$regiones),
              pilares          = normalize_regiones(input$pilares),
              usar_codigos     = TRUE,
              paleta           = input$paleta,
              mostrar_promedio = isTRUE(input$mostrar_promedio),
              promedio_shape   = as.integer(input$promedio_shape),
              promedio_size    = as.numeric(input$promedio_size),
              promedio_color   = input$promedio_color,
              promedio_fill    = input$promedio_fill,
              jitter_width     = as.numeric(input$jitter_width),
              jitter_height    = as.numeric(input$jitter_height)
            )
            if (inherits(g,"ggplot")) print(g) else grid::grid.draw(g)
          }
        )
        list(src=pngfile,contentType="image/png",width="100%",deleteFile=TRUE)
      },deleteFile=TRUE)
    })
    
    output$dl_png <- downloadHandler(
      filename = function() paste0("general_dispersion_", input$edicion, ".png"),
      content = function(file) {
        .save_png_hq(file, plot_fun=function(){
          g <- incorer::general_dispersion_pilares(
            edicion          = as.integer(input$edicion),
            regiones         = normalize_regiones(input$regiones),
            pilares          = normalize_regiones(input$pilares),
            usar_codigos     = TRUE,
            paleta           = input$paleta,
            mostrar_promedio = isTRUE(input$mostrar_promedio),
            promedio_shape   = as.integer(input$promedio_shape),
            promedio_size    = as.numeric(input$promedio_size),
            promedio_color   = input$promedio_color,
            promedio_fill    = input$promedio_fill,
            jitter_width     = as.numeric(input$jitter_width),
            jitter_height    = as.numeric(input$jitter_height)
          )
          if (inherits(g,"ggplot")) print(g) else grid::grid.draw(g)
        })
      }
    )
  })
}


# ─────────────────────────────────────────────────────────────
# MÓDULO: GENERAL ▸ DISTRIBUCIÓN (incorer::general_distribucion)
# ─────────────────────────────────────────────────────────────
general_distribucion_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        div(class = "sticky-sidebar",
            h4("Parámetros"),
            # Edición (categoría por año)
            selectInput(ns("edicion"), "Edición (año)", choices = 2016:2025, selected = 2025),
            
            # Modo: "pilares" (6 ejes) o "general" (1 eje GEN)
            selectInput(ns("modo"), "Modo",
                        choices = c("Pilares" = "pilares", "General (GEN)" = "general"),
                        selected = "pilares"),
            
            # Regiones (multi) con ALL exclusivo
            selectizeInput(ns("regiones"), "Regiones",
                           choices  = c("Todas (ALL)" = "ALL"),
                           selected = "ALL", multiple = TRUE,
                           options  = list(plugins = list("remove_button"),
                                           placeholder = "Selecciona regiones o ALL")),
            
            # Tipo de distribución y extras
            selectInput(ns("tipo"), "Tipo de distribución",
                        choices = c("boxplot", "violin"),
                        selected = "boxplot"),
            checkboxInput(ns("jitter"), "Superponer puntos (jitter)", value = FALSE),
            
            # Paleta y capas auxiliares
            selectInput(ns("paleta"), "Paleta de relleno",
                        choices = c("blues", "viridis", "cividis"),
                        selected = "blues"),
            checkboxInput(ns("linea_promedio"), "Línea de promedio nacional", value = TRUE),
            checkboxInput(ns("mostrar_leyenda"), "Mostrar leyenda", value = FALSE),
            
            # Botones (uno debajo del otro, centrados)
            div(class = "btn-center-vertical",
                actionButton(ns("go"), "Generar gráfico", class = "btn btn-primary"),
                downloadButton(ns("dl_png"), "Descargar PNG", class = "btn btn-primary")
            )
        )
      ),
      column(
        width = 8,
        div(class = "result-card",
            div(class = "ldf-plot",
                imageOutput(ns("img"), height = "auto")
            )
        )
      )
    )
  )
}

general_distribucion_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # — Catálogo de regiones (tolerante a nombres de columnas) —
    cat_r <- tryCatch(incorer::catalogo_region(), error = function(e) NULL)
    reg_choices <- c("Todas (ALL)" = "ALL")
    if (!is.null(cat_r)) {
      if (all(c("codigo","nombre") %in% names(cat_r))) {
        reg_choices <- c(reg_choices, stats::setNames(cat_r$codigo, cat_r$nombre))
      } else if (all(c("region","region_nombre") %in% names(cat_r))) {
        reg_choices <- c(reg_choices, stats::setNames(cat_r$region, cat_r$region_nombre))
      }
    }
    updateSelectizeInput(session, "regiones", choices = reg_choices, selected = "ALL", server = TRUE)
    
    # — Exclusividad de "ALL" en regiones —
    observeEvent(input$regiones, ignoreInit = TRUE, {
      v <- input$regiones
      if (is.null(v)) return()
      if ("ALL" %in% v && length(v) > 1) updateSelectizeInput(session, "regiones", selected = "ALL")
      if (!("ALL" %in% v) && length(v) == 0) updateSelectizeInput(session, "regiones", selected = "ALL")
    })
    
    # — Utilidades internas para render nítido —
    safe_num1 <- function(x, default){
      x <- suppressWarnings(as.numeric(x))
      if (length(x) != 1 || is.na(x) || !is.finite(x)) return(default)
      x
    }
    
    # Guardamos los últimos parámetros para la descarga en alta resolución
    last_params <- reactiveVal(NULL)
    
    # — Render del PNG (retina, responsive) al presionar "Generar gráfico" —
    observeEvent(input$go, {
      # Congruencia de parámetros
      edicion  <- as.integer(input$edicion)
      regiones <- if (is.null(input$regiones) || "ALL" %in% input$regiones) "ALL" else unique(input$regiones)
      modo     <- input$modo
      tipo     <- input$tipo
      
      # persistimos para download
      last_params(list(
        edicion = edicion, regiones = regiones, modo = modo, tipo = tipo,
        jitter = isTRUE(input$jitter), paleta = input$paleta,
        linea_promedio = isTRUE(input$linea_promedio),
        mostrar_leyenda = isTRUE(input$mostrar_leyenda)
      ))
      
      output$img <- renderImage({
        pngfile <- tempfile(fileext = ".png")
        err <- NULL
        
        # Ancho en CSS del contenedor de la imagen (namespaced + fallback)
        w_css <- NULL
        key_ns <- paste0("output_", session$ns("img"), "_width")
        key_gl <- "output_img_width"
        if (!is.null(session$clientData[[key_ns]])) w_css <- session$clientData[[key_ns]]
        if (is.null(w_css) && !is.null(session$clientData[[key_gl]])) w_css <- session$clientData[[key_gl]]
        w_css <- safe_num1(w_css, 1000); if (w_css <= 0) w_css <- 1000
        
        # Device Pixel Ratio -> mínimo 2× para nitidez
        dpr <- safe_num1(input$.__dpr__, 1)
        oversample <- if (isTRUE(dpr >= 1)) max(dpr, 2) else 2
        
        # Dimensiones y resolución
        w_px <- as.integer(round(w_css * oversample)); if (w_px <= 0) w_px <- 2000
        h_px <- as.integer(round(w_px * 0.62));        if (h_px <= 0) h_px <- 1240
        res_ <- 96 * oversample
        
        ragg::agg_png(pngfile, width = w_px, height = h_px, units = "px", res = res_, background = "white")
        try({
          op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
          par(mar = c(5, 22, 3, 2), xaxs = "i", yaxs = "i")
          
          g <- tryCatch(
            incorer::general_distribucion(
              edicion         = edicion,
              modo            = modo,               # "pilares" o "general"
              regiones        = regiones,           # "ALL" o vector de códigos/nombres/grupos
              usar_codigos    = TRUE,               # siempre TRUE (oculto)
              tipo            = tipo,               # "boxplot" o "violin"
              jitter          = isTRUE(input$jitter),
              paleta          = input$paleta,       # "blues","viridis","cividis"
              linea_promedio  = isTRUE(input$linea_promedio),
              mostrar_leyenda = isTRUE(input$mostrar_leyenda)
            ),
            error = function(e){ err <<- conditionMessage(e); NULL }
          )
          
          if (inherits(g, "ggplot")) {
            print(g)
          } else if (inherits(g, "grob") || inherits(g, "gTree")) {
            grid::grid.newpage(); grid::grid.draw(g)
          } else if (is.function(g)) {
            g()
          } else if (!is.null(err)) {
            stop(err)
          }
        }, silent = TRUE)
        dev.off()
        
        list(src = pngfile, contentType = "image/png", width = "100%",
             alt = "general_distribucion()", deleteFile = TRUE)
      }, deleteFile = TRUE)
    }, ignoreInit = TRUE)
    
    # — Descargar PNG en alta resolución (re-render dedicado) —
    output$dl_png <- downloadHandler(
      filename = function(){
        paste0("general_distribucion_", Sys.Date(), ".png")
      },
      content = function(file){
        p <- last_params()
        # Si aún no hay parámetros (no han clickeado), usamos defaults visibles
        if (is.null(p)) {
          p <- list(
            edicion = as.integer(input$edicion),
            regiones = if (is.null(input$regiones) || "ALL" %in% input$regiones) "ALL" else unique(input$regiones),
            modo = input$modo, tipo = input$tipo,
            jitter = isTRUE(input$jitter), paleta = input$paleta,
            linea_promedio = isTRUE(input$linea_promedio),
            mostrar_leyenda = isTRUE(input$mostrar_leyenda)
          )
        }
        
        # Tamaño “publicación”: ancho 3000px (ajusta si deseas)
        w_px <- 3000L
        h_px <- as.integer(round(w_px * 0.62))
        res_ <- 320  # DPI efectivo alto
        
        ragg::agg_png(file, width = w_px, height = h_px, units = "px", res = res_, background = "white")
        op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
        par(mar = c(5, 22, 3, 2), xaxs = "i", yaxs = "i")
        g <- incorer::general_distribucion(
          edicion         = p$edicion,
          modo            = p$modo,
          regiones        = p$regiones,
          usar_codigos    = TRUE,
          tipo            = p$tipo,
          jitter          = p$jitter,
          paleta          = p$paleta,
          linea_promedio  = p$linea_promedio,
          mostrar_leyenda = p$mostrar_leyenda
        )
        if (inherits(g, "ggplot")) {
          print(g)
        } else if (inherits(g, "grob") || inherits(g, "gTree")) {
          grid::grid.newpage(); grid::grid.draw(g)
        } else if (is.function(g)) {
          g()
        }
        dev.off()
      }
    )
  })
}


# ─────────────────────────────────────────────────────────────
# UI: GENERAL ▸ HEATMAP
# ─────────────────────────────────────────────────────────────
general_heatmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        div(class = "sticky-sidebar",
            h4("Parámetros"),
            
            # Rango de ediciones (dos thumbs)
            sliderInput(
              inputId = ns("ediciones"),
              label   = "Ediciones (rango)",
              min     = 2016, max = 2025,
              value   = c(2021, 2025),
              step    = 1, sep = ""
            ),
            
            # Regiones (multi) con ALL exclusivo
            selectizeInput(
              ns("regiones"), "Regiones",
              choices  = c("Todas (ALL)" = "ALL"),
              selected = "ALL", multiple = TRUE,
              options  = list(
                plugins     = list("remove_button"),
                placeholder = "Selecciona regiones o ALL"
              )
            ),
            
            selectInput(ns("ordenar"), "Ordenar regiones por",
                        choices = c("Ninguno" = "ninguno",
                                    "Última edición" = "por_ultimo",
                                    "Promedio" = "por_promedio"),
                        selected = "ninguno"),
            
            selectInput(ns("paleta"), "Paleta",
                        choices = c("blues","viridis","cividis","magma"),
                        selected = "blues"),
            
            checkboxInput(ns("anotar"),          "Anotar valores",     value = FALSE),
            checkboxInput(ns("mostrar_leyenda"), "Mostrar leyenda",    value = TRUE),
            
            # Botones: grandes, centrados, en columna
            div(class = "btn-center-vertical",
                actionButton(ns("go"),      "Generar gráfico",  class = "btn btn-primary"),
                downloadButton(ns("dl_png"), "Descargar PNG",    class = "btn btn-primary")
            )
        )
      ),
      
      column(
        width = 8,
        div(class = "result-card",
            div(class = "ldf-plot",
                imageOutput(ns("img"), height = "auto")
            )
        )
      )
    )
  )
}


# ─────────────────────────────────────────────────────────────
# SERVER: GENERAL ▸ HEATMAP
# ─────────────────────────────────────────────────────────────
general_heatmap_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # ── Catálogo de regiones y ALL exclusivo ─────────────────
    cat_r <- tryCatch(incorer::catalogo_region(), error = function(e) NULL)
    reg_choices <- c("Todas (ALL)" = "ALL")
    if (!is.null(cat_r)) {
      if (all(c("codigo","nombre") %in% names(cat_r))) {
        reg_choices <- c(reg_choices, stats::setNames(cat_r$codigo, cat_r$nombre))
      } else if (all(c("region","region_nombre") %in% names(cat_r))) {
        reg_choices <- c(reg_choices, stats::setNames(cat_r$region, cat_r$region_nombre))
      }
    }
    updateSelectizeInput(session, "regiones", choices = reg_choices, selected = "ALL", server = TRUE)
    
    observeEvent(input$regiones, ignoreInit = TRUE, {
      r <- input$regiones
      if (is.null(r)) return()
      if ("ALL" %in% r && length(r) > 1) updateSelectizeInput(session, "regiones", selected = "ALL")
      if (!("ALL" %in% r) && length(r) == 0) updateSelectizeInput(session, "regiones", selected = "ALL")
    })
    
    # ── Helpers seguros ──────────────────────────────────────
    safe_num1 <- function(x, default){
      x <- suppressWarnings(as.numeric(x))
      if (length(x)!=1 || is.na(x) || !is.finite(x)) return(default)
      x
    }
    # fuerza un vector de 2+ ediciones a partir del slider
    ediciones_seq <- function(rango, min_year = 2016, max_year = 2025){
      if (length(rango) < 2) return(min_year:max_year)
      a <- max(min_year, min(rango))
      b <- min(max_year, max(rango))
      if (a == b) b <- min(max_year, a + 1)  # garantiza 2+
      seq(a, b, by = 1)
    }
    regiones_norm <- function(v){
      if (is.null(v) || length(v) == 0 || "ALL" %in% v) return("ALL")
      unique(v)
    }
    
    # guardamos últimos parámetros para descarga
    rv <- reactiveValues(last_params = NULL)
    
    # ── Render (PNG retina con ragg) ─────────────────────────
    observeEvent(input$go, {
      output$img <- renderImage({
        pngfile <- tempfile(fileext = ".png")
        err <- NULL
        
        # ancho del contenedor (namespaced + fallback)
        w_css <- NULL
        key_ns <- paste0("output_", session$ns("img"), "_width")
        key_gl <- "output_img_width"
        if (!is.null(session$clientData[[key_ns]])) w_css <- session$clientData[[key_ns]]
        if (is.null(w_css) && !is.null(session$clientData[[key_gl]])) w_css <- session$clientData[[key_gl]]
        w_css <- safe_num1(w_css, 1000); if (w_css <= 0) w_css <- 1000
        
        # oversampling para nitidez (mínimo 2×)
        dpr <- safe_num1(input$.__dpr__, 1)
        oversample <- if (isTRUE(dpr >= 1)) max(dpr, 2) else 2
        
        w_px <- as.integer(round(w_css * oversample)); if (w_px <= 0) w_px <- 2000
        h_px <- as.integer(round(w_px * 0.62));        if (h_px <= 0) h_px <- 1240
        res_ <- 96 * oversample
        
        # parámetros
        eds  <- ediciones_seq(input$ediciones)
        regs <- regiones_norm(input$regiones)
        ord  <- input$ordenar
        pal  <- input$paleta
        
        # guardar para descarga
        rv$last_params <- list(
          ediciones = eds, regiones = regs, ordenar = ord,
          paleta = pal, anotar = isTRUE(input$anotar),
          mostrar_leyenda = isTRUE(input$mostrar_leyenda)
        )
        
        # render
        ragg::agg_png(pngfile, width = w_px, height = h_px, units = "px",
                      res = res_, background = "white")
        try({
          g <- tryCatch(
            incorer::general_heatmap(
              ediciones       = eds,
              regiones        = regs,
              usar_codigos    = TRUE,
              ordenar         = ord,
              paleta          = pal,
              anotar          = isTRUE(input$anotar),
              mostrar_leyenda = isTRUE(input$mostrar_leyenda)
            ),
            error = function(e){ err <<- conditionMessage(e); NULL }
          )
          if (inherits(g, "ggplot")) {
            print(g)
          } else if (!is.null(err)) {
            stop(err)
          }
        }, silent = TRUE)
        dev.off()
        
        list(src = pngfile, contentType = "image/png", width = "100%",
             alt = "general_heatmap()", deleteFile = TRUE)
      }, deleteFile = TRUE)
    }, ignoreInit = TRUE)
    
    # ── Descargar PNG (alta resolución) ───────────────────────
    output$dl_png <- downloadHandler(
      filename = function(){
        rng <- input$ediciones
        paste0("general_heatmap_", rng[1], "-", rng[2], ".png")
      },
      content = function(file){
        p <- rv$last_params
        if (is.null(p)) {
          # Si aún no hicieron "Generar gráfico", usamos valores actuales
          p <- list(
            ediciones = ediciones_seq(input$ediciones),
            regiones  = regiones_norm(input$regiones),
            ordenar   = input$ordenar,
            paleta    = input$paleta,
            anotar    = isTRUE(input$anotar),
            mostrar_leyenda = isTRUE(input$mostrar_leyenda)
          )
        }
        # Alta resolución fija (muy nítido)
        w_px <- 3200; h_px <- as.integer(round(w_px * 0.62)); res_ <- 300
        ragg::agg_png(file, width = w_px, height = h_px, units = "px",
                      res = res_, background = "white")
        print(
          incorer::general_heatmap(
            ediciones       = p$ediciones,
            regiones        = p$regiones,
            usar_codigos    = TRUE,
            ordenar         = p$ordenar,
            paleta          = p$paleta,
            anotar          = p$anotar,
            mostrar_leyenda = p$mostrar_leyenda
          )
        )
        dev.off()
      }
    )
  })
}



# ─────────────────────────────────────────────────────────────
# UI — navbar por familias + bienvenida
# ─────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "INCOREr — La Datafora",
  id = "topnav",
  header = tagList(
    tags$head(
      includeCSS("ladatafora.css"),
      tags$style(css_extras),
      tags$script(copy_js),
      tags$script(dpr_js),
      tags$script(HTML("Shiny.addCustomMessageHandler('copyScriptById', function(id){ copyScriptById(id); });"))
    )
  ),

  tabPanel("Bienvenida",
    fluidPage(
      br(),
      h2("INCOREr — Explorador (Beta)"),
      p("La interfaz organiza funciones en cuatro familias (General, Indicadores, Valores, Bivariado). Cada bloque es un módulo aislado."),
      tags$ul(
        tags$li("Usar la barra superior para elegir la familia."),
        tags$li("Dentro de cada familia, seleccionar la función."),
        tags$li("Los botones están unificados y centrados.")
      ),
      br(),
      div(class="result-card", style="padding:14px;",
          strong("Estado: "),
          p("Implementados: General ▸ Barras, Tabla, Distribución, Dispersión por pilar.")
      ),
      br()
    )
  ),

  navbarMenu("General",
    tabPanel("Barras",               general_barras_ui("general_barras")),
    tabPanel("Distribución",         general_distribucion_ui("general_distrib")),
    tabPanel("Dispersión", general_dispersion_pilares_ui("general_disp")),
    tabPanel("Mapa de calor", general_heatmap_ui("general_heatmap"))
  ),

  navbarMenu("Indicadores",
    tabPanel("Pronto",
      placeholder_ui("indc_placeholder", "Indicadores", "Próximamente: indc_dispersion(), indc_distribucion(), etc.")
    )
  ),

  navbarMenu("Valores",
    tabPanel("Pronto",
      placeholder_ui("valor_placeholder", "Valores originales", "Próximamente: valor_dispersion(), valor_distribucion(), etc.")
    )
  ),

  navbarMenu("Bivariado",
    tabPanel("Pronto",
      placeholder_ui("bivalor_placeholder", "Análisis bivariado", "Próximamente: comparaciones/correlaciones entre indicadores.")
    )
  )
)

# ─────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session){
  # General
  general_barras_server("general_barras")
  general_distribucion_server("general_distrib")
  general_dispersion_pilares_server("general_disp")
  general_heatmap_server("general_heatmap")

  # Placeholders
  placeholder_server("indc_placeholder")
  placeholder_server("valor_placeholder")
  placeholder_server("bivalor_placeholder")
}


shinyApp(ui, server)

