box::use(
  shiny[
    NS,
    moduleServer,
    selectInput,
    sliderInput,
    uiOutput,
    textOutput,
    renderUI,
    renderText,
    tags,
    reactive,
    req
  ],
  bslib[input_switch]
)

mod_acoustic_telemetry_sidebar_ui <- function(
  id,
  show_toggle = TRUE,
  show_raw_layer = FALSE
) {
  ns <- NS(id)

  tags$div(
    class = "acoustic-telemetry-sidebar",
    if (show_raw_layer) {
      input_switch(
        ns("show_raw_data"),
        "Show acoustic telemetry raw data",
        value = FALSE
      )
    },
    if (show_toggle) {
      input_switch(
        ns("show_network"),
        "Show acoustic telemetry network",
        value = FALSE
      )
    },
    uiOutput(ns("raw_month_picker")),
    uiOutput(ns("month_slider")),
    tags$div(
      style = "text-align:center; font-weight:bold;",
      textOutput(ns("month_label"))
    )
  )
}

mod_acoustic_telemetry_sidebar_server <- function(
  id,
  months,
  show_toggle = TRUE,
  raw_months = NULL,
  show_raw_layer = FALSE
) {
  moduleServer(id, function(input, output, session) {
    months <- sort(unique(as.Date(months)))
    raw_months <- sort(unique(as.integer(raw_months)))
    raw_months <- raw_months[!is.na(raw_months)]

    output$raw_month_picker <- renderUI({
      if (!show_raw_layer || !isTRUE(input$show_raw_data)) {
        return(NULL)
      }

      if (length(raw_months) == 0) {
        return(tags$div("No acoustic telemetry raw detections available."))
      }

      selectInput(
        session$ns("raw_month"),
        "Raw data month",
        choices = stats::setNames(raw_months, month.name[raw_months]),
        selected = raw_months[1]
      )
    })

    output$month_slider <- renderUI({
      if (show_toggle && !isTRUE(input$show_network)) {
        return(NULL)
      }

      if (length(months) == 0 || all(is.na(months))) {
        return(tags$div("No acoustic telemetry detections available."))
      }

      sliderInput(
        session$ns("month_range"),
        "Year-month range",
        min = months[1],
        max = months[length(months)],
        value = c(months[1], months[length(months)]),
        timeFormat = "%Y-%m",
        step = 30,
        ticks = FALSE
      )
    })

    selected_month_range <- reactive({
      if (show_toggle && !isTRUE(input$show_network)) {
        return(NULL)
      }

      if (length(months) == 0 || all(is.na(months))) {
        return(NULL)
      }

      req(input$month_range)

      month_range <- months[c(
        selected_month_index(input$month_range[1], months, 1L),
        selected_month_index(input$month_range[2], months, length(months))
      )]

      sort(month_range)
    })

    output$month_label <- renderText({
      month_range <- selected_month_range()

      if (is.null(month_range)) {
        return("")
      }

      format_month_range(month_range)
    })

    selected_raw_month <- reactive({
      if (
        !show_raw_layer ||
          !isTRUE(input$show_raw_data) ||
          length(raw_months) == 0
      ) {
        return(NULL)
      }

      if (is.null(input$raw_month)) {
        return(raw_months[1])
      }

      selected_month <- suppressWarnings(as.integer(input$raw_month))
      if (
        length(selected_month) == 0 ||
          is.na(selected_month) ||
          !selected_month %in% raw_months
      ) {
        return(raw_months[1])
      }

      selected_month
    })

    reactive({
      month_range <- selected_month_range()
      raw_month <- selected_raw_month()

      list(
        show_network = if (!is.null(month_range) && show_toggle) {
          isTRUE(input$show_network)
        } else {
          !is.null(month_range)
        },
        show_raw_data = if (!is.null(raw_month) && show_raw_layer) {
          isTRUE(input$show_raw_data)
        } else {
          FALSE
        },
        month_range = month_range,
        month_label = if (is.null(month_range)) {
          ""
        } else {
          format_month_range(month_range)
        },
        raw_month = raw_month,
        raw_month_label = if (is.null(raw_month)) {
          ""
        } else {
          month.name[raw_month]
        }
      )
    })
  })
}

selected_month_index <- function(input_value, months, default_index) {
  selected_month <- tryCatch(
    as.Date(input_value),
    error = function(e) NA
  )

  if (length(selected_month) == 0 || is.na(selected_month)) {
    return(default_index)
  }

  month_index <- match(
    format(selected_month, "%Y-%m"),
    format(months, "%Y-%m")
  )
  if (!is.na(month_index)) {
    return(month_index)
  }

  which.min(abs(as.numeric(months - selected_month)))
}

format_month_range <- function(month_range) {
  month_labels <- format(month_range, "%Y-%m")

  if (identical(month_labels[1], month_labels[2])) {
    return(month_labels[1])
  }

  paste(month_labels, collapse = " to ")
}
