# Africa Soil Diversity Atlas, server.R

server <- function(input, output, session) {
  # 1. Reactive state
  clicked_point <- reactiveVal(NULL)
  diversity_vals <- reactiveVal(NULL)

  # Throttle mouse-move extractions to max 1 per 80 ms
  mouse_throttle <- reactiveVal(Sys.time())

  # 2. Base map, rendered once
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = TRUE,
      zoomControlPosition = "topright"
    )) |>
      addProviderTiles(
        provider = "CartoDB.DarkMatter",
        layerId = "basemap",
        options = providerTileOptions(noWrap = TRUE)
      ) |>
      setView(lng = 20, lat = -5, zoom = 4) |>
      fitBounds(
        lng1 = COUNTRY_BBOX$ALL$lng1, lat1 = COUNTRY_BBOX$ALL$lat1,
        lng2 = COUNTRY_BBOX$ALL$lng2, lat2 = COUNTRY_BBOX$ALL$lat2
      ) |>
      addRasterImage(
        x = rasters$mean[["q1"]],
        colors = colorNumeric("YlGn", domain = NULL, na.color = "transparent"),
        opacity = 0.75,
        layerId = "diversity_layer"
      )
  })

  # 3. Basemap swap
  observeEvent(input$basemap, {
    leafletProxy("map") |>
      addProviderTiles(
        provider = input$basemap,
        layerId = "basemap",
        options = providerTileOptions(noWrap = TRUE)
      )
  })

  # 4. Diversity layer swap
  observeEvent(input$hill_order, {
    lyr <- input$hill_order
    meta <- LAYER_META[[lyr]]
    rng <- LAYER_RANGES[[lyr]]

    pal <- colorNumeric(
      palette = meta$palette,
      domain = c(rng$min, rng$max),
      na.color = "transparent"
    )

    leafletProxy("map") |>
      addRasterImage(
        x = rasters$mean[[lyr]],
        colors = pal,
        opacity = 0.75,
        layerId = "diversity_layer"
      )

    update_legend()
    if (!is.null(diversity_vals())) render_result_cards(diversity_vals())
  })

  # 5. Country filter, fly to bounding box
  observeEvent(input$country_filter, {
    bb <- COUNTRY_BBOX[[input$country_filter]]
    leafletProxy("map") |>
      flyToBounds(
        lng1 = bb$lng1, lat1 = bb$lat1,
        lng2 = bb$lng2, lat2 = bb$lat2
      )
  })

  # 6. Mouse move, live tooltip popover
  observeEvent(input$map_mousemove, {
    
    # Throttle: skip if last update was <80 ms ago
    now <- Sys.time()
    if (as.numeric(now - mouse_throttle(), units = "secs") < 0.08) return()
    mouse_throttle(now)

    mv <- input$map_mousemove
    if (is.null(mv)) return()

    vals <- extract_diversity(mv$lng, mv$lat)

    if (is.null(vals)) {
      session$sendCustomMessage("updateTooltip", list(visible = FALSE))
      return()
    }

    lyr <- isolate(input$hill_order)
    meta <- LAYER_META[[lyr]]

    session$sendCustomMessage("updateTooltip", list(
      visible = TRUE,
      label = meta$short,
      mean = format_diversity(vals$mean[[lyr]],  lyr),
      ppi90 = format_diversity(vals$ppi90[[lyr]], lyr),
      unit = meta$unit,
      lat = round(mv$lat, 4),
      lng = round(mv$lng, 4)
    ))
  })

  # 7. Map click, pin point & fill results panel
  observeEvent(input$map_click, {
    click <- input$map_click
    clicked_point(click)

    vals <- extract_diversity(click$lng, click$lat)
    diversity_vals(vals)

    if (is.null(vals)) {
      showNotification(
        "No data at this location. Please click within the study area.",
        type = "warning",
        duration = 4
      )
      return()
    }

    leafletProxy("map") |>
      clearGroup("click_marker") |>
      addCircleMarkers(
        lng = click$lng,
        lat = click$lat,
        radius = 7,
        color = "#b8dba8",
        weight = 2,
        fillColor = "#7aaa6a",
        fillOpacity = 0.9,
        group = "click_marker",
        # Accessible popup as fallback for screen readers
        popup = sprintf(
          "<b>%.4f°, %.4f°</b>",
          click$lat, click$lng
        )
      )

    session$sendCustomMessage("showResults", list())
    render_coords(click$lng, click$lat)
    render_result_cards(vals)
  })

  # 8. Output renderers
  # 8a. Coordinates badge
  render_coords <- function(lng, lat) {
    output$coords_display <- renderUI({
      tags$span(
        class = "coords-badge",
        sprintf("%.4f\u00b0  %.4f\u00b0", lat, lng)
      )
    })
  }

  # 8b. Hill cards (mean ± ppi90)
  render_result_cards <- function(vals) {
    render_one <- function(output_id, layer_key) {
      output[[output_id]] <- renderUI({
        mean_str <- format_diversity(vals$mean[[layer_key]], layer_key)
        ppi90_str <- format_diversity(vals$ppi90[[layer_key]], layer_key)
        tagList(
          tags$span(class = "hill-value", mean_str),
          tags$span(class = "hill-uncertainty", paste0("\u00b1 ", ppi90_str))
        )
      })
    }
    render_one("val_q0", "q0")
    render_one("val_q1", "q1")
    render_one("val_q2", "q2")
    render_one("val_even", "logE")
  }

  # 8c. Legend
  update_legend <- function() {
    lyr <- isolate(input$hill_order)
    meta <- LAYER_META[[lyr]]
    rng <- LAYER_RANGES[[lyr]]
    unit_str <- if (nzchar(meta$unit)) paste0(" (", meta$unit, ")") else ""

    output$legend_title <- renderUI({
      tags$span(class = "legend-title", paste0(meta$label, unit_str))
    })
    output$legend_min <- renderUI({ tags$span(format_diversity(rng$min, lyr)) })
    output$legend_max <- renderUI({ tags$span(format_diversity(rng$max, lyr)) })
  }

  update_legend()

  # 9. Placeholder initialisers (suppress "output not found" warnings)
  output$coords_display <- renderUI({ NULL })

  for (oid in c("val_q0", "val_q1", "val_q2", "val_even")) {
    local({
      id <- oid
      output[[id]] <- renderUI({
        tags$span(class = "hill-value", "\u2014")
      })
    })
  }
}
