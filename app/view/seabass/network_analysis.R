box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    uiOutput,
    tags,
    tagList,
    renderUI,
    req
  ],
  bslib[layout_sidebar, sidebar],
  leaflet[
    leafletOutput,
    renderLeaflet,
    addCircleMarkers,
    addControl,
    addLegend,
    colorNumeric,
    labelOptions
  ],
  leaflet.minicharts[addFlows, popupArgs],
  htmltools[HTML],
  app / logic / maps[make_base_map],
  app / view / acoustic_telemetry_sidebar[
    mod_acoustic_telemetry_sidebar_ui,
    mod_acoustic_telemetry_sidebar_server,
    format_month_range
  ],
  dplyr[
    arrange,
    count,
    desc,
    distinct,
    filter,
    group_by,
    left_join,
    lead,
    mutate,
    n,
    n_distinct,
    rename,
    slice_head,
    summarise,
    transmute,
    ungroup
  ]
)

# ui ----------------------------------------------------------------------

mod_seabass_network_analysis_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      open = "open",
      title = "Network analysis",
      mod_acoustic_telemetry_sidebar_ui(ns("controls"), show_toggle = FALSE),
      uiOutput(ns("month_summary"))
    ),
    leafletOutput(ns("network_map"), height = 1000)
  )
}

# server ------------------------------------------------------------------

mod_seabass_network_analysis_server <- function(
  id,
  detections
) {
  moduleServer(id, function(input, output, session) {
    network_data <- prep_network_analysis_data(detections)
    months <- network_data$months
    pal <- colorNumeric(
      palette = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
      domain = detection_domain(network_data$detections_monthly),
      na.color = "#d9d9d9"
    )
    max_flow <- flow_domain(network_data$flows)

    range_selection <- mod_acoustic_telemetry_sidebar_server(
      "controls",
      months = months,
      show_toggle = FALSE
    )

    current_month_range <- reactive({
      req(range_selection()$month_range)
      range_selection()$month_range
    })

    current_detections <- reactive({
      detections_for_month_range(
        network_data$detections_monthly,
        current_month_range()
      )
    })

    current_station_points <- reactive({
      station_points_for_range(
        network_data$detections_monthly,
        network_data$stations,
        current_month_range()
      )
    })

    current_flows <- reactive({
      flow_lines_for_range(network_data$flows, current_month_range())
    })

    output$month_summary <- renderUI({
      month_range <- current_month_range()
      range_label <- format_month_range(month_range)
      detections_range <- current_detections()
      stations_range <- current_station_points()
      flows_range <- current_flows()

      if (nrow(detections_range) == 0) {
        return(tagList(
          tags$hr(),
          tags$b(range_label),
          tags$div("No detections for this range.")
        ))
      }

      top_station <- stations_range |>
        arrange(desc(n_detections)) |>
        slice_head(n = 1)

      top_flow <- flows_range |>
        arrange(desc(n_transitions)) |>
        slice_head(n = 1)

      top_station_label <- if (nrow(top_station) == 0) {
        "-"
      } else {
        paste0(
          top_station$station_name[1],
          " (n = ",
          top_station$n_detections[1],
          ")"
        )
      }

      top_flow_label <- if (nrow(top_flow) == 0) {
        "-"
      } else {
        paste0(
          top_flow$from[1],
          " -> ",
          top_flow$to[1],
          " (n = ",
          top_flow$n_transitions[1],
          ")"
        )
      }

      tagList(
        tags$hr(),
        tags$b(range_label),
        tags$ul(
          tags$li(tagList(
            "Total detections: ",
            tags$strong(nrow(detections_range))
          )),
          tags$li(tagList(
            "Individuals detected: ",
            tags$strong(n_distinct(detections_range$animal_id))
          )),
          tags$li(tagList(
            "Active stations: ",
            tags$strong(n_distinct(detections_range$station_name))
          )),
          tags$li(tagList(
            "Movements between stations: ",
            tags$strong(sum(flows_range$n_transitions, na.rm = TRUE))
          )),
          tags$li(tagList(
            "Station with most detections: ",
            tags$strong(top_station_label)
          )),
          tags$li(tagList(
            "Strongest flow: ",
            tags$strong(top_flow_label)
          ))
        )
      )
    })

    output$network_map <- renderLeaflet({
      stations_m <- current_station_points()
      flows_m <- current_flows()

      map <- make_base_map() |>
        addCircleMarkers(
          data = network_data$stations,
          lng = ~lon,
          lat = ~lat,
          radius = 3,
          stroke = FALSE,
          fillOpacity = 0.55,
          fillColor = "#6b7280",
          popup = ~paste0("<b>", station_name, "</b>")
        )

      if (nrow(flows_m) > 0) {
        map <- map |>
          addFlows(
            lng0 = flows_m$from_lon,
            lat0 = flows_m$from_lat,
            lng1 = flows_m$to_lon,
            lat1 = flows_m$to_lat,
            flow = flows_m$n_transitions,
            color = "#111827",
            opacity = 0.65,
            minThickness = 1,
            maxThickness = 8,
            maxFlow = max_flow,
            popup = popupArgs(labels = flows_m$popup)
          )
      }

      if (nrow(stations_m) > 0) {
        map <- map |>
          addCircleMarkers(
            data = stations_m,
            lng = ~lon,
            lat = ~lat,
            radius = ~radius,
            stroke = TRUE,
            color = "#111827",
            weight = 1,
            fillOpacity = 0.85,
            fillColor = ~pal(n_detections),
            popup = ~popup,
            label = ~as.character(n_detections),
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
            pal = pal,
            values = detection_domain(network_data$detections_monthly),
            title = "Detections",
            opacity = 1
          )
      }

      map |>
        addControl(
          html = HTML(paste0(
            "<div style='background:white;padding:6px 8px;",
            "box-shadow:0 1px 4px rgba(0,0,0,0.25);'>",
            "<b>Network analysis</b><br>",
            "Point colour and label show detections per station.<br>",
            "Flow width shows transitions between stations in the selected range.",
            "</div>"
          )),
          position = "bottomright"
        )
    })
  })
}

network_analysis_data_cache <- new.env(parent = emptyenv())

prep_network_analysis_data <- function(detections) {
  latest_detection <- suppressWarnings(
    max(as.numeric(detections$date_time), na.rm = TRUE)
  )
  cache_key <- paste(
    nrow(detections),
    ncol(detections),
    latest_detection,
    sep = ":"
  )
  if (exists(cache_key, envir = network_analysis_data_cache, inherits = FALSE)) {
    return(get(cache_key, envir = network_analysis_data_cache))
  }

  detections_monthly <- detections |>
    filter(
      !is.na(date_time),
      !is.na(station_name),
      !is.na(deploy_latitude),
      !is.na(deploy_longitude)
    ) |>
    mutate(
      month = as.Date(paste0(format(as.Date(date_time), "%Y-%m"), "-01"))
    )

  stations <- detections_monthly |>
    group_by(station_name) |>
    summarise(
      lat = mean(deploy_latitude, na.rm = TRUE),
      lon = mean(deploy_longitude, na.rm = TRUE),
      .groups = "drop"
    ) |>
    distinct(station_name, lon, lat) |>
    arrange(station_name)

  movement_events <- detections_monthly |>
    arrange(animal_id, date_time) |>
    group_by(animal_id) |>
    mutate(
      to = lead(station_name),
      to_month = lead(month)
    ) |>
    ungroup() |>
    filter(
      !is.na(to),
      station_name != to
    ) |>
    transmute(
      from_month = month,
      to_month,
      from = station_name,
      to
    )

  from_coords <- stations |>
    rename(
      from = station_name,
      from_lat = lat,
      from_lon = lon
    )

  to_coords <- stations |>
    rename(
      to = station_name,
      to_lat = lat,
      to_lon = lon
    )

  flows <- movement_events |>
    left_join(from_coords, by = "from") |>
    left_join(to_coords, by = "to") |>
    filter(
      !is.na(from_lat),
      !is.na(from_lon),
      !is.na(to_lat),
      !is.na(to_lon)
    )

  result <- list(
    detections_monthly = detections_monthly,
    stations = stations,
    flows = flows,
    months = sort(unique(detections_monthly$month))
  )

  assign(cache_key, result, envir = network_analysis_data_cache)
  result
}

detections_for_month_range <- function(detections_monthly, month_range) {
  detections_monthly |>
    filter(
      month >= month_range[1],
      month <= month_range[2]
    )
}

station_points_for_range <- function(detections_monthly, stations, month_range) {
  stations_range <- detections_for_month_range(detections_monthly, month_range) |>
    group_by(station_name) |>
    summarise(
      n_detections = n(),
      n_individuals = n_distinct(animal_id),
      .groups = "drop"
    ) |>
    left_join(stations, by = "station_name") |>
    arrange(station_name)

  if (nrow(stations_range) == 0) {
    return(stations_range)
  }

  range_label <- format_month_range(month_range)

  stations_range |>
    mutate(
      radius = scale_between(sqrt(n_detections), 7, 24),
      popup = paste0(
        "<b>",
        station_name,
        "</b><br><b>Range:</b> ",
        range_label,
        "<br><b>Detections:</b> ",
        n_detections,
        "<br><b>Individuals:</b> ",
        n_individuals
      )
    )
}

flow_lines_for_range <- function(flows, month_range) {
  flows_range <- flows |>
    filter(
      from_month >= month_range[1],
      from_month <= month_range[2],
      to_month >= month_range[1],
      to_month <= month_range[2]
    ) |>
    group_by(from, to, from_lat, from_lon, to_lat, to_lon) |>
    summarise(n_transitions = n(), .groups = "drop") |>
    arrange(desc(n_transitions))

  if (nrow(flows_range) == 0) {
    return(flows_range)
  }

  range_label <- format_month_range(month_range)

  flows_range |>
    mutate(
      popup = paste0(
        "<b>From:</b> ",
        from,
        "<br><b>To:</b> ",
        to,
        "<br><b>Transitions:</b> ",
        n_transitions,
        "<br><b>Range:</b> ",
        range_label
      )
    )
}

detection_domain <- function(detections_monthly) {
  if (nrow(detections_monthly) == 0) {
    return(c(0, 1))
  }

  station_totals <- detections_monthly |>
    group_by(station_name) |>
    summarise(n_detections = n(), .groups = "drop")

  station_totals$n_detections
}

flow_domain <- function(flows) {
  if (nrow(flows) == 0) {
    return(1)
  }

  flow_totals <- flows |>
    count(from, to, name = "n_transitions")

  max(flow_totals$n_transitions, 1, na.rm = TRUE)
}

scale_between <- function(x, min_value, max_value) {
  if (length(x) == 0) {
    return(numeric(0))
  }

  x_range <- range(x, na.rm = TRUE)
  if (!all(is.finite(x_range)) || x_range[1] == x_range[2]) {
    return(rep(mean(c(min_value, max_value)), length(x)))
  }

  min_value +
    ((x - x_range[1]) / (x_range[2] - x_range[1])) *
      (max_value - min_value)
}
