box::use(
  shiny[NS, moduleServer, observeEvent, reactiveVal],
  leaflet[
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    clearGroup,
    clearImages,
    addCircleMarkers,
    addRasterImage,
    addControl,
    addLegend,
    colorNumeric,
    labelOptions,
    removeControl
  ],
  leaflet.minicharts[addFlows, clearFlows, popupArgs],
  bslib[layout_sidebar, sidebar],
  dplyr[
    arrange,
    desc,
    filter,
    group_by,
    left_join,
    mutate,
    n,
    n_distinct,
    summarise
  ],
  app / view / environmental_filters[mod_env_filters_ui, mod_env_filters_server],
  app / view / habitat_suitability_sidebar[habitat_suitability_selected_layer],
  app / view / seabass / network_analysis[
    prep_network_analysis_data,
    station_points_for_range,
    flow_lines_for_range,
    detection_domain,
    flow_domain,
    scale_between
  ],
  htmltools[HTML],
  app / logic / config[bioflow_duc2_url]
)

box::use(
  app / logic / maps[make_base_map, make_env_base_map, update_env_wms_map],
)
mod_env_ui <- function(id, wms_layers) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      open = "desktop",
      title = "Layers",
      mod_env_filters_ui(
        ns("filters"),
        wms_layers = wms_layers,
        bioflow_duc2_url = bioflow_duc2_url
      )
    ),
    leafletOutput(ns("env_map"), height = 600)
  )
}


mod_env_server <- function(
  id,
  wms_layers,
  habitat_data = NULL,
  acoustic_detections = NULL,
  pam_data = NULL
) {
  moduleServer(id, function(input, output, session) {
    acoustic_network_data <- if (is.null(acoustic_detections)) {
      NULL
    } else {
      prep_network_analysis_data(acoustic_detections)
    }
    acoustic_months <- if (is.null(acoustic_network_data)) {
      as.Date(character(0))
    } else {
      acoustic_network_data$months
    }
    acoustic_raw_data <- if (is.null(acoustic_network_data)) {
      NULL
    } else {
      prep_acoustic_raw_month_data(
        acoustic_network_data$detections_monthly,
        acoustic_network_data$stations
      )
    }
    acoustic_raw_months <- if (is.null(acoustic_raw_data)) {
      integer(0)
    } else {
      acoustic_raw_data$months
    }
    acoustic_pal <- if (is.null(acoustic_network_data)) {
      NULL
    } else {
      colorNumeric(
        palette = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
        domain = detection_domain(acoustic_network_data$detections_monthly),
        na.color = "#d9d9d9"
      )
    }
    acoustic_raw_pal <- if (is.null(acoustic_raw_data)) {
      NULL
    } else {
      colorNumeric(
        palette = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
        domain = raw_detection_domain(acoustic_raw_data),
        na.color = "#d9d9d9"
      )
    }
    acoustic_max_flow <- if (is.null(acoustic_network_data)) {
      1
    } else {
      flow_domain(acoustic_network_data$flows)
    }
    pam_summary_data <- if (is.null(pam_data)) {
      NULL
    } else {
      prep_pam_summary_data(pam_data)
    }
    pam_months <- if (is.null(pam_summary_data)) {
      integer(0)
    } else {
      pam_summary_data$months
    }
    pam_pal <- colorNumeric(
      palette = c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe", "#084081"),
      domain = pam_ratio_domain(pam_summary_data),
      na.color = "#d9d9d9"
    )

    selected_layers <- mod_env_filters_server(
      "filters",
      habitat_data = habitat_data,
      acoustic_months = acoustic_months,
      acoustic_raw_months = acoustic_raw_months,
      pam_months = pam_months
    )
    active_layers <- reactiveVal(character(0))

    output$env_map <- renderLeaflet({
      make_env_base_map(base_map = make_base_map, wms_layers = wms_layers)
    })

    observeEvent(selected_layers(),
      {
        current_layers <- update_env_wms_map(
          map_id = "env_map",
          session = session,
          wms_layers = wms_layers,
          selected_layers = selected_layers(),
          cached_layers = active_layers()
        )

        active_layers(current_layers)
      },
      ignoreNULL = FALSE
    )

    observeEvent(selected_layers()$habitat,
      {
        proxy <- leafletProxy("env_map", session = session) |>
          clearImages() |>
          removeControl("legend-habitat-suitability")

        layer <- habitat_suitability_selected_layer(
          habitat_data = habitat_data,
          selection = selected_layers()$habitat
        )

        if (is.null(layer)) {
          return(invisible(NULL))
        }

        proxy |>
          addRasterImage(
            layer$raster,
            colors = layer$palette,
            opacity = 0.75,
            layerId = "habitat_suitability_raster"
          ) |>
          addLegend(
            pal = layer$palette,
            values = layer$domain,
            title = layer$title,
            layerId = "legend-habitat-suitability"
          )
      },
      ignoreNULL = FALSE
    )

    observeEvent(selected_layers()$acoustic_telemetry,
      {
        selection <- selected_layers()$acoustic_telemetry

        proxy <- clear_acoustic_telemetry_overlays(
          leafletProxy("env_map", session = session)
        )

        if (is.null(acoustic_network_data) || is.null(selection)) {
          return(invisible(NULL))
        }

        if (
          isTRUE(selection$show_raw_data) &&
            !is.null(acoustic_raw_data) &&
            !is.null(selection$raw_month)
        ) {
          raw_points <- raw_station_points_for_month(
            acoustic_raw_data,
            selection$raw_month
          )

          proxy <- proxy |>
            addCircleMarkers(
              data = acoustic_raw_data$stations,
              lng = ~lon,
              lat = ~lat,
              radius = 3,
              stroke = FALSE,
              fillOpacity = 0.55,
              fillColor = "#6b7280",
              group = "Acoustic telemetry raw receiver stations",
              popup = ~paste0("<b>", station_name, "</b>")
            )

          if (nrow(raw_points) > 0) {
            proxy <- proxy |>
              addCircleMarkers(
                data = raw_points,
                lng = ~lon,
                lat = ~lat,
                radius = ~radius,
                stroke = TRUE,
                color = "#003f5c",
                weight = 1,
                fillOpacity = 0.85,
                fillColor = ~acoustic_raw_pal(n_detections),
                popup = ~popup,
                label = ~as.character(n_detections),
                group = "Acoustic telemetry raw detections",
                labelOptions = labelOptions(
                  noHide = TRUE,
                  direction = "center",
                  textOnly = TRUE,
                  style = list(
                    "font-weight" = "700",
                    "font-size" = "11px",
                    "color" = "#111827"
                  )
                )
              ) |>
              addLegend(
                position = "bottomleft",
                pal = acoustic_raw_pal,
                values = raw_detection_domain(acoustic_raw_data),
                title = "Raw detections",
                opacity = 1,
                layerId = "legend-acoustic-raw-data"
              )
          }

          proxy <- proxy |>
            addControl(
              html = HTML(paste0(
                "<div style='background:white;padding:6px 8px;",
                "box-shadow:0 1px 4px rgba(0,0,0,0.25);'>",
                "<b>Acoustic telemetry raw data</b><br>",
                "Point colour and label show detections per station.<br>",
                "Detections are combined across years for ",
                selection$raw_month_label,
                ".",
                "</div>"
              )),
              position = "bottomright",
              layerId = "control-acoustic-raw-data"
            )
        }

        if (
          isTRUE(selection$show_network) &&
            !is.null(selection$month_range)
        ) {
          stations_range <- station_points_for_range(
            acoustic_network_data$detections_monthly,
            acoustic_network_data$stations,
            selection$month_range
          )
          flows_range <- flow_lines_for_range(
            acoustic_network_data$flows,
            selection$month_range
          )

          proxy <- proxy |>
            addCircleMarkers(
              data = acoustic_network_data$stations,
              lng = ~lon,
              lat = ~lat,
              radius = 3,
              stroke = FALSE,
              fillOpacity = 0.55,
              fillColor = "#6b7280",
              group = "Acoustic telemetry network receiver stations",
              popup = ~paste0("<b>", station_name, "</b>")
            )

          if (nrow(flows_range) > 0) {
            proxy <- proxy |>
              addFlows(
                lng0 = flows_range$from_lon,
                lat0 = flows_range$from_lat,
                lng1 = flows_range$to_lon,
                lat1 = flows_range$to_lat,
                flow = flows_range$n_transitions,
                color = "#111827",
                opacity = 0.65,
                minThickness = 1,
                maxThickness = 8,
                maxFlow = acoustic_max_flow,
                popup = popupArgs(labels = flows_range$popup)
              )
          }

          if (nrow(stations_range) > 0) {
            proxy <- proxy |>
              addCircleMarkers(
                data = stations_range,
                lng = ~lon,
                lat = ~lat,
                radius = ~radius,
                stroke = TRUE,
                color = "#111827",
                weight = 1,
                fillOpacity = 0.85,
                fillColor = ~acoustic_pal(n_detections),
                popup = ~popup,
                label = ~as.character(n_detections),
                group = "Acoustic telemetry network detections",
                labelOptions = labelOptions(
                  noHide = TRUE,
                  direction = "center",
                  textOnly = TRUE,
                  style = list(
                    "font-weight" = "700",
                    "font-size" = "11px",
                    "color" = "#111827"
                  )
                )
              ) |>
              addLegend(
                position = "bottomleft",
                pal = acoustic_pal,
                values = detection_domain(acoustic_network_data$detections_monthly),
                title = "Acoustic detections",
                opacity = 1,
                layerId = "legend-acoustic-network"
              )
          }

          proxy |>
            addControl(
              html = HTML(paste0(
                "<div style='background:white;padding:6px 8px;",
                "box-shadow:0 1px 4px rgba(0,0,0,0.25);'>",
                "<b>Acoustic telemetry network</b><br>",
                "Point colour and label show detections per station.<br>",
                "Flow width shows transitions between stations in the selected range.",
                "</div>"
              )),
              position = "bottomright",
              layerId = "control-acoustic-network"
            )
        }
      },
      ignoreNULL = FALSE
    )

    observeEvent(selected_layers()$passive_acoustic_monitoring,
      {
        selection <- selected_layers()$passive_acoustic_monitoring

        proxy <- clear_pam_summary_overlay(
          leafletProxy("env_map", session = session)
        )

        if (
          is.null(pam_summary_data) ||
            is.null(selection) ||
            !isTRUE(selection$show_summary) ||
            is.null(selection$month)
        ) {
          return(invisible(NULL))
        }

        pam_points <- pam_points_for_month(
          pam_summary_data,
          selection$month
        )

        proxy <- proxy |>
          addCircleMarkers(
            data = pam_summary_data$stations,
            lng = ~lon,
            lat = ~lat,
            radius = 3,
            stroke = FALSE,
            fillOpacity = 0.55,
            fillColor = "#6b7280",
            group = "PAM stations",
            popup = ~paste0("<b>", Station, "</b>")
          )

        if (nrow(pam_points) > 0) {
          proxy <- proxy |>
            addCircleMarkers(
              data = pam_points,
              lng = ~lon,
              lat = ~lat,
              radius = ~radius,
              stroke = TRUE,
              color = "#08306b",
              weight = 1,
              fillOpacity = 0.85,
              fillColor = ~pam_pal(positive_ratio),
              popup = ~popup,
              label = ~ratio_label,
              group = "PAM positive-hour ratio",
              labelOptions = labelOptions(
                noHide = TRUE,
                direction = "center",
                textOnly = TRUE,
                style = list(
                  "font-weight" = "700",
                  "font-size" = "10px",
                  "color" = "#111827"
                )
              )
            ) |>
            addLegend(
              position = "bottomleft",
              pal = pam_pal,
              values = pam_ratio_domain(pam_summary_data),
              title = "Positive-hour ratio",
              opacity = 1,
              layerId = "legend-pam-summary"
            )
        }

        proxy |>
          addControl(
            html = HTML(paste0(
              "<div style='background:white;padding:6px 8px;",
              "box-shadow:0 1px 4px rgba(0,0,0,0.25);'>",
              "<b>Passive acoustic monitoring</b><br>",
              "Bubble colour, size and label show positive-hour ratio.<br>",
              "Ratio = sum(PPM) / monitored hours for ",
              selection$month_label,
              ".",
              "</div>"
            )),
            position = "bottomright",
            layerId = "control-pam-summary"
          )
      },
      ignoreNULL = FALSE
    )

    selected_layers
  })
}

clear_acoustic_telemetry_overlays <- function(map) {
  map <- map |>
    clearFlows() |>
    clearGroup("Acoustic telemetry receiver stations") |>
    clearGroup("Acoustic telemetry detections") |>
    clearGroup("Acoustic telemetry network receiver stations") |>
    clearGroup("Acoustic telemetry network detections") |>
    clearGroup("Acoustic telemetry raw receiver stations") |>
    clearGroup("Acoustic telemetry raw detections") |>
    removeControl("legend-acoustic-telemetry") |>
    removeControl("control-acoustic-telemetry") |>
    removeControl("legend-acoustic-network") |>
    removeControl("control-acoustic-network") |>
    removeControl("legend-acoustic-raw-data") |>
    removeControl("control-acoustic-raw-data")

  map
}

clear_pam_summary_overlay <- function(map) {
  map <- map |>
    clearGroup("PAM stations") |>
    clearGroup("PAM positive-hour ratio") |>
    removeControl("legend-pam-summary") |>
    removeControl("control-pam-summary")

  map
}

acoustic_raw_data_cache <- new.env(parent = emptyenv())
pam_summary_data_cache <- new.env(parent = emptyenv())

prep_acoustic_raw_month_data <- function(detections_monthly, stations) {
  cache_key <- paste(nrow(detections_monthly), nrow(stations), sep = ":")
  if (exists(cache_key, envir = acoustic_raw_data_cache, inherits = FALSE)) {
    return(get(cache_key, envir = acoustic_raw_data_cache))
  }

  detections_by_month <- detections_monthly |>
    mutate(month_of_year = as.integer(format(month, "%m")))

  station_months <- detections_by_month |>
    group_by(month_of_year, station_name) |>
    summarise(
      n_detections = n(),
      n_individuals = n_distinct(animal_id),
      .groups = "drop"
    ) |>
    left_join(stations, by = "station_name") |>
    filter(
      !is.na(lat),
      !is.na(lon)
    ) |>
    arrange(month_of_year, station_name)

  month_totals <- station_months |>
    group_by(month_of_year) |>
    summarise(n_month = sum(n_detections, na.rm = TRUE), .groups = "drop")

  station_months <- station_months |>
    left_join(month_totals, by = "month_of_year") |>
    mutate(
      n_month = pmax(n_month, 1),
      rel = n_detections / n_month
    )

  result <- list(
    stations = stations,
    station_months = station_months,
    months = sort(unique(station_months$month_of_year))
  )

  assign(cache_key, result, envir = acoustic_raw_data_cache)
  result
}

raw_station_points_for_month <- function(raw_data, month_value) {
  station_month <- raw_data$station_months |>
    filter(month_of_year == month_value) |>
    arrange(desc(n_detections))

  if (nrow(station_month) == 0) {
    return(station_month)
  }

  month_label <- month.name[month_value]

  station_month |>
    mutate(
      radius = scale_between(sqrt(n_detections), 7, 24),
      rel_percent = 100 * rel,
      popup = paste0(
        "<b>",
        station_name,
        "</b><br><b>Month:</b> ",
        month_label,
        "<br><b>Detections:</b> ",
        n_detections,
        "<br><b>Individuals:</b> ",
        n_individuals,
        "<br><b>Share of month detections:</b> ",
        sprintf("%.1f%%", rel_percent)
      )
    )
}

raw_detection_domain <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data$station_months) == 0) {
    return(c(0, 1))
  }

  raw_data$station_months$n_detections
}

prep_pam_summary_data <- function(pam_data) {
  cache_key <- paste(nrow(pam_data), ncol(pam_data), sep = ":")
  if (exists(cache_key, envir = pam_summary_data_cache, inherits = FALSE)) {
    return(get(cache_key, envir = pam_summary_data_cache))
  }

  pam_hourly <- pam_data |>
    filter(
      !is.na(datetime),
      !is.na(Station),
      !is.na(decLat),
      !is.na(decLon),
      !is.na(PPM)
    ) |>
    mutate(
      month_of_year = as.integer(format(as.Date(datetime), "%m"))
    )

  stations <- pam_hourly |>
    group_by(Station) |>
    summarise(
      lat = mean(decLat, na.rm = TRUE),
      lon = mean(decLon, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(
      !is.na(lat),
      !is.na(lon)
    ) |>
    arrange(Station)

  station_months <- pam_hourly |>
    group_by(month_of_year, Station) |>
    summarise(
      positive_hours = sum(PPM, na.rm = TRUE),
      detection_hours = n_distinct(datetime),
      .groups = "drop"
    ) |>
    left_join(stations, by = "Station") |>
    filter(
      detection_hours > 0,
      !is.na(lat),
      !is.na(lon)
    ) |>
    mutate(
      positive_ratio = pmin(
        pmax(positive_hours / detection_hours, 0),
        1
      )
    ) |>
    arrange(month_of_year, Station)

  result <- list(
    stations = stations,
    station_months = station_months,
    months = sort(unique(station_months$month_of_year))
  )

  assign(cache_key, result, envir = pam_summary_data_cache)
  result
}

pam_points_for_month <- function(pam_summary_data, month_value) {
  station_month <- pam_summary_data$station_months |>
    filter(month_of_year == month_value) |>
    arrange(desc(positive_ratio))

  if (nrow(station_month) == 0) {
    return(station_month)
  }

  month_label <- month.name[month_value]

  station_month |>
    mutate(
      radius = 7 + 17 * sqrt(positive_ratio),
      ratio_label = paste0(round(100 * positive_ratio), "%"),
      popup = paste0(
        "<b>",
        Station,
        "</b><br><b>Month:</b> ",
        month_label,
        "<br><b>Positive-hour ratio:</b> ",
        sprintf("%.1f%%", 100 * positive_ratio),
        "<br><b>Positive hours:</b> ",
        positive_hours,
        "<br><b>Monitored hours:</b> ",
        detection_hours
      )
    )
}

pam_ratio_domain <- function(pam_summary_data) {
  if (
    is.null(pam_summary_data) ||
      nrow(pam_summary_data$station_months) == 0
  ) {
    return(c(0, 1))
  }

  c(0, 1)
}
