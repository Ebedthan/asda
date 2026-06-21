# =============================================================================
# Africa Soil Diversity Atlas — ui.R
# Full-screen map layout with floating panels
# =============================================================================

library(shiny)
library(leaflet)
library(bslib)

ui <- fluidPage(

  # ── Meta & assets ──────────────────────────────────────────────────────────
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Africa Soil Diversity Atlas"),
    tags$link(rel = "stylesheet", type = "text/css", href = "atlas.css")
  ),

  # ── Loading overlay ────────────────────────────────────────────────────────
  tags$div(
    id = "loading-overlay",
    tags$div(class = "loading-title", "Africa Soil Diversity Atlas"),
    tags$div(class = "loading-bar-wrap",
      tags$div(class = "loading-bar")
    ),
    tags$div(class = "loading-sub", "Loading spatial layers\u2026")
  ),

  # ── Full-screen map ────────────────────────────────────────────────────────
  leafletOutput("map", width = "100%", height = "100vh"),

  # ── Cursor tooltip (follows mouse, populated by JS) ────────────────────────
  # FIX: live popover showing predicted value under cursor
  tags$div(
    id    = "map-tooltip",
    class = "map-tooltip hidden",
    tags$span(id = "tooltip-label",  class = "tooltip-label",  ""),
    tags$span(id = "tooltip-value",  class = "tooltip-value",  ""),
    tags$span(id = "tooltip-coords", class = "tooltip-coords", "")
  ),

  # ── Header / controls panel (top-left) ─────────────────────────────────────
  tags$div(
    id = "panel-header",
    class = "float-panel",

    tags$div(class = "atlas-title", "Africa Soil", tags$br(), "Diversity Atlas"),
    tags$div(class = "atlas-subtitle", "16S rRNA \u00b7 9 Countries \u00b7 810 Sites"),

    tags$hr(class = "panel-divider"),

    tags$span(class = "control-label-custom", "Diversity layer"),
    selectInput(
      inputId  = "hill_order",
      label    = NULL,
      choices  = c(
        "Species richness  (q = 0)" = "q0",
        "Shannon diversity (q = 1)" = "q1",
        "Simpson diversity (q = 2)" = "q2",
        "Evenness (log)"            = "logE"
      ),
      selected = "q1"
    ),

    tags$hr(class = "panel-divider"),

    tags$span(class = "control-label-custom", "Basemap"),
    radioButtons(
      inputId  = "basemap",
      label    = NULL,
      choices  = c(
        "Satellite" = "Esri.WorldImagery",
        "Terrain"   = "Esri.WorldShadedRelief",
        "Minimal"   = "CartoDB.DarkMatter"
      ),
      selected = "CartoDB.DarkMatter"
    ),

    tags$hr(class = "panel-divider"),

    tags$span(class = "control-label-custom", "Filter by country"),
    selectInput(
      inputId  = "country_filter",
      label    = NULL,
      choices  = c(
        "All countries" = "ALL",
        "Benin"         = "BJ",
        "Botswana"      = "BW",
        "Côte d'Ivoire" = "CI",
        "Kenya"         = "KE",
        "Mozambique"    = "MZ",
        "Namibia"       = "NA",
        "South Africa"  = "ZA",
        "Zambia"        = "ZM",
        "Zimbabwe"      = "ZW"
      ),
      selected = "ALL"
    )
  ),

  # ── Results panel (bottom-right) ───────────────────────────────────────────
  tags$div(
    id    = "panel-results",
    class = "float-panel hidden",

    uiOutput("coords_display"),

    tags$div(
      class = "hill-grid",

      tags$div(
        class = "hill-card",
        tags$div(class = "hill-order", "q = 0"),
        tags$div(class = "hill-value-wrap", uiOutput("val_q0")),
        tags$div(class = "hill-label", "Richness")
      ),
      tags$div(
        class = "hill-card",
        tags$div(class = "hill-order", "q = 1"),
        tags$div(class = "hill-value-wrap", uiOutput("val_q1")),
        tags$div(class = "hill-label", "Shannon")
      ),
      tags$div(
        class = "hill-card",
        tags$div(class = "hill-order", "q = 2"),
        tags$div(class = "hill-value-wrap", uiOutput("val_q2")),
        tags$div(class = "hill-label", "Simpson")
      ),
      tags$div(
        class = "hill-card",
        tags$div(class = "hill-order", "logE"),
        tags$div(class = "hill-value-wrap", uiOutput("val_even")),
        tags$div(class = "hill-label", "Evenness")
      )
    ),

    tags$p(class = "result-hint", "Click anywhere on the map to query")
  ),

  # ── Legend panel (bottom-left) ─────────────────────────────────────────────
  tags$div(
    id    = "panel-legend",
    class = "float-panel",

    uiOutput("legend_title"),
    tags$div(class = "legend-gradient"),
    tags$div(
      class = "legend-ticks",
      tags$span(uiOutput("legend_min")),
      tags$span(uiOutput("legend_max"))
    )
  ),

  # ── JavaScript ─────────────────────────────────────────────────────────────
  tags$script(HTML("

    // 1. Fade loading overlay on connect
    $(document).on('shiny:connected', function() {
      setTimeout(function() {
        var el = document.getElementById('loading-overlay');
        if (el) el.classList.add('fade-out');
      }, 400);
    });

    // 2. Reveal results panel on first click
    Shiny.addCustomMessageHandler('showResults', function(msg) {
      var el = document.getElementById('panel-results');
      if (el) el.classList.remove('hidden');
    });

    // 3. Live cursor tooltip
    // Receives: { visible, label, mean, ppi90, unit, lat, lng }
    var tooltip = null;

    document.addEventListener('DOMContentLoaded', function() {
      tooltip = document.getElementById('map-tooltip');

      // Move tooltip with the mouse
      document.getElementById('map').addEventListener('mousemove', function(e) {
        if (!tooltip || tooltip.classList.contains('hidden')) return;
        var x = e.clientX, y = e.clientY;
        // Keep tooltip from clipping right/bottom edges
        var tw = tooltip.offsetWidth  || 180;
        var th = tooltip.offsetHeight || 60;
        tooltip.style.left = (x + tw + 16 > window.innerWidth  ? x - tw - 10 : x + 14) + 'px';
        tooltip.style.top  = (y + th + 16 > window.innerHeight ? y - th - 10 : y + 14) + 'px';
      });

      document.getElementById('map').addEventListener('mouseleave', function() {
        if (tooltip) tooltip.classList.add('hidden');
      });
    });

    Shiny.addCustomMessageHandler('updateTooltip', function(msg) {
      if (!tooltip) tooltip = document.getElementById('map-tooltip');
      if (!tooltip) return;

      if (!msg.visible) {
        tooltip.classList.add('hidden');
        return;
      }

      var unit = msg.unit ? ' ' + msg.unit : '';
      document.getElementById('tooltip-label').textContent  = msg.label;
      document.getElementById('tooltip-value').textContent  = msg.mean + ' \u00b1 ' + msg.ppi90 + unit;
      document.getElementById('tooltip-coords').textContent = msg.lat + '\u00b0  ' + msg.lng + '\u00b0';
      tooltip.classList.remove('hidden');
    });

  "))
)
