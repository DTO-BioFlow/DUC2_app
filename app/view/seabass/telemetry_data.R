# source("./helpers/wrangle_acoustic_telemetry_data.R")

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
    updateSliderInput,
    tagList,
    tags,
    textOutput,
    renderText,
    req
  ],
  bslib[layout_sidebar, sidebar, layout_columns],
  leaflet[
    leafletOutput,
    renderLeaflet,
    addCircleMarkers,
    addControl,
    leafletProxy,
    clearGroup
  ],
  htmltools[HTML],
  dplyr[
    mutate,
    filter,
    summarise,
    pull,
    n_distinct,
    group_by,
    arrange,
    desc,
    slice_head
  ]
)

# ui ----------------------------------------------------------------------

mod_seabass_telemetry_ui <- function(id) {
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
    leafletOutput(ns("map"), height = 1000)
  )
}

# server ------------------------------------------------------------------

mod_seabass_telemetry_data_server <- function(
  id,
  prepped_data,
  etn_monthyear_individual_sum
) {
  moduleServer(id, function(input, output, session) {
    stations <- prepped_data$stations
    anim_df <- prepped_data$anim_df
    months <- prepped_data$months

    stopifnot(length(months) > 0)

    output$month_slider <- renderUI({
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
        i <- 1L
      } else {
        i <- which.min(abs(as.numeric(months - selected_month)))
      }
      i
    })

    current_month <- reactive({
      months[current_month_index()]
    })

    output$month_label <- renderText({
      format(current_month(), "%Y-%m")
    })

    bubble_data_for_month <- function(month_value) {
      anim_df |>
        filter(month == month_value, n_station > 0) |>
        mutate(
          rel = pmax(rel, 0),
          rel_percent = 100 * rel,
          radius = 5 + 45 * sqrt(rel),
          popup = paste0(
            "<b>",
            station_name,
            "</b><br>",
            format(month, "%Y-%m"),
            "<br>Detections: ",
            n_station,
            "<br>Share of month detections: ",
            sprintf("%.1f%%", rel_percent)
          )
        )
    }

    current_bubbles <- reactive({
      bubble_data_for_month(current_month())
    })

    output$month_summary <- renderUI({
      m <- current_month()

      df_m <- etn_monthyear_individual_sum |>
        mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) |>
        filter(month == m)

      if (nrow(df_m) == 0) {
        return(tagList(
          tags$hr(),
          tags$b(format(m, "%Y-%m")),
          tags$div("No detections for this month.")
        ))
      }

      n_indiv <- df_m |>
        filter(n_detections > 0) |>
        summarise(n = n_distinct(tag_serial_number), .groups = "drop") |>
        pull(n)
      if (length(n_indiv) == 0 || is.na(n_indiv[1])) {
        n_indiv <- 0
      } else {
        n_indiv <- as.integer(n_indiv[1])
      }

      total_det <- sum(df_m$n_detections, na.rm = TRUE)

      top_station_indiv <- df_m |>
        filter(n_detections > 0) |>
        group_by(station_name) |>
        summarise(n_indiv = n_distinct(tag_serial_number), .groups = "drop") |>
        arrange(desc(n_indiv)) |>
        slice_head(n = 1)

      top_indiv_name <- if (nrow(top_station_indiv) == 0) {
        "-"
      } else {
        top_station_indiv$station_name[1]
      }
      top_indiv_val <- if (nrow(top_station_indiv) == 0) {
        0
      } else {
        top_station_indiv$n_indiv[1]
      }

      top_station_det <- df_m |>
        group_by(station_name) |>
        summarise(n_det = sum(n_detections, na.rm = TRUE), .groups = "drop") |>
        arrange(desc(n_det)) |>
        slice_head(n = 1)

      top_det_name <- if (nrow(top_station_det) == 0) {
        "-"
      } else {
        top_station_det$station_name[1]
      }
      top_det_val <- if (nrow(top_station_det) == 0) {
        0
      } else {
        top_station_det$n_det[1]
      }

      tagList(
        tags$hr(),
        tags$b(format(m, "%Y-%m")),
        tags$ul(
          tags$li(tagList(
            "Number of individuals detected: ",
            tags$strong(n_indiv)
          )),
          tags$li(tagList(
            "Total detections this month: ",
            tags$strong(total_det)
          )),
          tags$li(tagList(
            "Station with most individuals: ",
            tags$strong(top_indiv_name),
            " (n = ",
            top_indiv_val,
            ")"
          )),
          tags$li(tagList(
            "Station with most detections: ",
            tags$strong(top_det_name),
            " (n = ",
            top_det_val,
            ")"
          ))
        )
      )
    })

box::use(
  app / logic / maps[make_base_map]
)
    output$map <- renderLeaflet({
      initial_bubbles <- bubble_data_for_month(months[1])

      map <- make_base_map() |>
        addCircleMarkers(
          data = stations,
          lng = ~lon,
          lat = ~lat,
          radius = 3,
          stroke = FALSE,
          fillOpacity = 0.75,
          fillColor = "#666666",
          group = "Receiver stations",
          popup = ~paste0("<b>", station_name, "</b>")
        ) |>
        addControl(
          html = HTML(
            paste0(
              "<div style='background:white;padding:6px 8px;",
              "box-shadow:0 1px 4px rgba(0,0,0,0.25);'>",
              "<b>Bubble map</b><br>",
              "Bubble area is proportional to each station's share ",
              "of detections in the selected month.",
              "</div>"
            )
          ),
          position = "bottomright"
        )

      if (nrow(initial_bubbles) == 0) {
        return(map)
      }

      map |>
        addCircleMarkers(
          data = initial_bubbles,
          lng = ~lon,
          lat = ~lat,
          radius = ~radius,
          stroke = TRUE,
          color = "#003f5c",
          weight = 1,
          fillOpacity = 0.65,
          fillColor = "#2c7fb8",
          group = "Detection proportion",
          popup = ~popup
        )
    })

    observeEvent(current_month(), {
      data <- current_bubbles()

      proxy <- leafletProxy("map", session = session) |>
        clearGroup("Detection proportion")

      if (nrow(data) == 0) {
        return()
      }

      proxy |>
        addCircleMarkers(
          data = data,
          lng = ~lon,
          lat = ~lat,
          radius = ~radius,
          stroke = TRUE,
          color = "#003f5c",
          weight = 1,
          fillOpacity = 0.65,
          fillColor = "#2c7fb8",
          group = "Detection proportion",
          popup = ~popup
        )
    }, ignoreInit = TRUE)

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
