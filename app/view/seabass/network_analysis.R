box::use(
  shiny[
    NS,
    actionButton,
    sliderInput,
    uiOutput,
    moduleServer,
    observeEvent,
    reactive,
    renderUI,
    textOutput,
    renderText,
    updateSliderInput,
    tags,
    tagList,
    req
  ],
  bslib[layout_sidebar, sidebar, layout_columns],
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
      layout_columns(
        actionButton(ns("prev_month"), "Previous month", width = "100%"),
        actionButton(ns("next_month"), "Next month", width = "100%"),
        col_widths = c(6, 6)
      ),
      uiOutput(ns("month_slider")),
      tags$div(
        style = "text-align:center; font-weight:bold;",
        textOutput(ns("month_label"))
      ),
      uiOutput(ns("month_summary"))
    ),
    leafletOutput(ns("network_map"), height = 1000)
  )
}

# server ------------------------------------------------------------------

mod_seabass_network_analysis_server <- function(
  id,
  detections,
  base_map_fun
) {
  moduleServer(id, function(input, output, session) {
    network_data <- prep_network_analysis_data(detections)
    months <- network_data$months
    pal <- colorNumeric(
      palette = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
      domain = detection_domain(network_data$stations_month),
      na.color = "#d9d9d9"
    )
    max_flow <- max(network_data$flows$n_transitions, 1, na.rm = TRUE)

    output$month_slider <- renderUI({
      if (length(months) == 0) {
        return(tags$div("No acoustic telemetry detections available."))
      }

      sliderInput(
        session$ns("month"),
        "Year-month",
        min = months[1],
        max = months[length(months)],
        value = months[1],
        timeFormat = "%Y-%m",
        step = 30,
        ticks = FALSE
      )
    })

    current_month_index <- reactive({
      req(input$month)

      selected_month <- as.Date(input$month)
      if (is.na(selected_month)) {
        1L
      } else {
        which.min(abs(as.numeric(months - selected_month)))
      }
    })

    current_month <- reactive({
      req(length(months) > 0)
      months[current_month_index()]
    })

    current_station_points <- reactive({
      station_points_for_month(
        network_data$stations_month,
        current_month()
      )
    })

    current_flows <- reactive({
      flow_lines_for_month(network_data$flows, current_month())
    })

    output$month_label <- renderText({
      format(current_month(), "%Y-%m")
    })

    output$month_summary <- renderUI({
      m <- current_month()
      stations_m <- current_station_points()
      flows_m <- current_flows()
      summary_m <- network_data$month_summary |>
        filter(month == m)

      if (nrow(summary_m) == 0) {
        return(tagList(
          tags$hr(),
          tags$b(format(m, "%Y-%m")),
          tags$div("No detections for this month.")
        ))
      }

      top_station <- stations_m |>
        arrange(desc(n_detections)) |>
        slice_head(n = 1)

      top_flow <- flows_m |>
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
        tags$b(format(m, "%Y-%m")),
        tags$ul(
          tags$li(tagList(
            "Total detections: ",
            tags$strong(summary_m$total_detections[1])
          )),
          tags$li(tagList(
            "Individuals detected: ",
            tags$strong(summary_m$n_individuals[1])
          )),
          tags$li(tagList(
            "Active stations: ",
            tags$strong(summary_m$active_stations[1])
          )),
          tags$li(tagList(
            "Movements between stations: ",
            tags$strong(sum(flows_m$n_transitions, na.rm = TRUE))
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

      map <- base_map_fun() |>
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
            values = detection_domain(network_data$stations_month),
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
            "Flow width shows transitions between stations.",
            "</div>"
          )),
          position = "bottomright"
        )
    })

    observeEvent(input$prev_month, {
      i <- max(1, current_month_index() - 1)
      updateSliderInput(
        session,
        "month",
        value = months[i]
      )
    })

    observeEvent(input$next_month, {
      i <- min(length(months), current_month_index() + 1)
      updateSliderInput(
        session,
        "month",
        value = months[i]
      )
    })
  })
}

prep_network_analysis_data <- function(detections) {
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

  stations_month <- detections_monthly |>
    group_by(month, station_name) |>
    summarise(
      n_detections = n(),
      n_individuals = n_distinct(animal_id),
      .groups = "drop"
    ) |>
    left_join(stations, by = "station_name") |>
    arrange(month, station_name)

  movement_events <- detections_monthly |>
    arrange(animal_id, date_time) |>
    group_by(animal_id) |>
    mutate(
      to = lead(station_name),
      to_month = lead(month)
    ) |>
    ungroup() |>
    filter(
      month == to_month,
      !is.na(to),
      station_name != to
    ) |>
    transmute(
      month,
      from = station_name,
      to
    ) |>
    count(month, from, to, name = "n_transitions")

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
    ) |>
    arrange(month, desc(n_transitions)) |>
    mutate(
      popup = paste0(
        "<b>From:</b> ",
        from,
        "<br><b>To:</b> ",
        to,
        "<br><b>Transitions:</b> ",
        n_transitions,
        "<br><b>Month:</b> ",
        format(month, "%Y-%m")
      )
    )

  month_summary <- detections_monthly |>
    group_by(month) |>
    summarise(
      total_detections = n(),
      n_individuals = n_distinct(animal_id),
      active_stations = n_distinct(station_name),
      .groups = "drop"
    )

  list(
    stations = stations,
    stations_month = stations_month,
    flows = flows,
    month_summary = month_summary,
    months = sort(unique(detections_monthly$month))
  )
}

station_points_for_month <- function(stations_month, month_value) {
  stations_m <- stations_month |>
    filter(month == month_value)

  if (nrow(stations_m) == 0) {
    return(stations_m)
  }

  stations_m |>
    mutate(
      radius = scale_between(sqrt(n_detections), 7, 24),
      popup = paste0(
        "<b>",
        station_name,
        "</b><br><b>Month:</b> ",
        format(month, "%Y-%m"),
        "<br><b>Detections:</b> ",
        n_detections,
        "<br><b>Individuals:</b> ",
        n_individuals
      )
    )
}

flow_lines_for_month <- function(flows, month_value) {
  flows |>
    filter(month == month_value)
}

detection_domain <- function(stations_month) {
  if (nrow(stations_month) == 0) {
    return(c(0, 1))
  }

  stations_month$n_detections
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
