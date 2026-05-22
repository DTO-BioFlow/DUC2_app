box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    renderUI,
    selectInput,
    tags,
    uiOutput
  ],
  bslib[input_switch]
)

mod_passive_acoustic_monitoring_sidebar_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "passive-acoustic-monitoring-sidebar",
    input_switch(
      ns("show_summary"),
      "Show PAM positive-hour ratio",
      value = FALSE
    ),
    uiOutput(ns("month_picker"))
  )
}

mod_passive_acoustic_monitoring_sidebar_server <- function(
  id,
  months = NULL
) {
  moduleServer(id, function(input, output, session) {
    months <- sort(unique(as.integer(months)))
    months <- months[!is.na(months)]

    output$month_picker <- renderUI({
      if (!isTRUE(input$show_summary)) {
        return(NULL)
      }

      if (length(months) == 0) {
        return(tags$div("No passive acoustic monitoring data available."))
      }

      selectInput(
        session$ns("month"),
        "Month",
        choices = stats::setNames(months, month.name[months]),
        selected = months[1]
      )
    })

    selected_month <- reactive({
      if (!isTRUE(input$show_summary) || length(months) == 0) {
        return(NULL)
      }

      if (is.null(input$month)) {
        return(months[1])
      }

      month_value <- suppressWarnings(as.integer(input$month))
      if (
        length(month_value) == 0 ||
          is.na(month_value) ||
          !month_value %in% months
      ) {
        return(months[1])
      }

      month_value
    })

    reactive({
      month <- selected_month()

      list(
        show_summary = isTRUE(input$show_summary) && !is.null(month),
        month = month,
        month_label = if (is.null(month)) "" else month.name[month]
      )
    })
  })
}
