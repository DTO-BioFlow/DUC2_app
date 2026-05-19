##################################################################################
##################################################################################

# Author: Assistant
# Date: 2026-05-04
# Script Name: ~/DUC2_app/app/view/habitat_suitability.R
# Script Description: Habitat suitability raster display with layer toggles

##################################################################################
##################################################################################

box::use(
  shiny[
    NS,
    moduleServer,
    radioButtons,
    selectInput,
    sliderInput,
    textOutput,
    renderText,
    tags,
    observe,
    reactive,
    req,
    updateSliderInput,
    isolate
  ],
  bslib[layout_sidebar, sidebar],
  leaflet[
    leafletOutput,
    renderLeaflet,
    setView,
    addRasterImage,
    addLegend,
    leafletProxy,
    clearImages,
    clearControls
  ],
  terra[values, nlyr]
)

# ui ----------------------------------------------------------------------

mod_habitat_suitability_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      open = "open",
      title = "Habitat suitability",
      radioButtons(
        ns("period"),
        "Layer",
        choices = c(
          "Monthly" = "present_monthly",
          "Decadal (2000-2019)" = "present_decade",
          "Decadal climate scenarios (2020-2100)" = "future_decade"
        ),
        selected = "present_monthly"
      ),
      selectInput(
        ns("species"),
        "Species",
        choices = c(
          "Harbour porpoise" = "harbour_porpoise",
          "Bottlenose dolphin" = "bottlenose_dolphin",
          "Common dolphin" = "common_dolphin",
          "Harbour seal" = "harbour_seal"
        ),
        selected = "harbour_porpoise"
      ),
      sliderInput(
        ns("time_index"),
        "Month",
        min = 1,
        max = 12,
        value = 1,
        step = 1,
        ticks = FALSE
      ),
      tags$div(
        style = "text-align:center; font-weight:bold;",
        textOutput(ns("time_label"))
      )
    ),
    leafletOutput(ns("habitat_map"), height = 700)
  )
}

box::use(
  app / logic / maps[make_base_map]
)
# server  -----------------------------------------------------------------

mod_habitat_suitability_server <- function(
  id,
  habitat_data = NULL,
  sidebar_layers = NULL
) {
  moduleServer(id, function(input, output, session) {
    present_decade_labels <- c("2000-2009", "2010-2019")
    future_decade_labels <- c(
      "2020-2029",
      "2030-2039",
      "2040-2049",
      "2050-2059",
      "2060-2069",
      "2070-2079",
      "2080-2089",
      "2090-2099",
      "2100"
    )

    selected_layer_id <- reactive({
      req(input$species, input$period)
      paste(input$species, input$period, sep = "_")
    })

    current_info <- reactive({
      req(habitat_data, selected_layer_id())
      habitat_data$habitat_layers_info[[selected_layer_id()]]
    })

    current_raster_stack <- reactive({
      req(habitat_data, selected_layer_id())
      habitat_data$habitat_layers[[selected_layer_id()]]
    })

    current_palette <- reactive({
      req(habitat_data, selected_layer_id())
      habitat_data$habitat_palettes[[selected_layer_id()]]
    })

    fallback_layer_count <- function(period) {
      if (period == "present_monthly") {
        return(12)
      }

      if (period == "present_decade") {
        return(length(present_decade_labels))
      }

      length(future_decade_labels)
    }

    raster_time_label <- function(r_stack, period, index) {
      if (!is.null(r_stack)) {
        layer_name <- names(r_stack)[index]
        if (
          length(layer_name) == 1 &&
            !is.na(layer_name) &&
            nzchar(layer_name)
        ) {
          return(layer_name)
        }
      }

      if (period == "present_monthly") {
        return(month.name[index])
      }

      if (period == "present_decade" && index <= length(present_decade_labels)) {
        return(present_decade_labels[index])
      }

      if (period == "future_decade" && index <= length(future_decade_labels)) {
        return(future_decade_labels[index])
      }

      paste("Layer", index)
    }

    current_layer_count <- reactive({
      req(input$period)
      r_stack <- current_raster_stack()

      if (is.null(r_stack)) {
        return(fallback_layer_count(input$period))
      }

      max(1, nlyr(r_stack))
    })

    observe({
      req(input$period)
      max_value <- current_layer_count()
      current_value <- isolate(input$time_index)

      if (is.null(current_value)) {
        current_value <- 1
      }

      updateSliderInput(
        session,
        "time_index",
        label = if (input$period == "present_monthly") "Month" else "Decade",
        min = 1,
        max = max_value,
        value = min(current_value, max_value),
        step = 1
      )
    })

    output$time_label <- renderText({
      req(input$period, input$time_index)
      raster_time_label(
        r_stack = current_raster_stack(),
        period = input$period,
        index = input$time_index
      )
    })

    # Initialize base map
    output$habitat_map <- renderLeaflet({
      make_base_map() |>
        setView(lat = 51.5, lng = 2.5, zoom = 8)
    })

    observe({
      req(input$time_index)
      proxy <- leafletProxy("habitat_map", session = session) |>
        clearImages() |>
        clearControls()

      r_stack <- current_raster_stack()
      pal <- current_palette()

      if (is.null(r_stack) || is.null(pal)) {
        return(invisible(NULL))
      }

      layer_index <- min(input$time_index, nlyr(r_stack))
      r <- r_stack[[layer_index]]
      info <- current_info()
      time_label <- raster_time_label(r_stack, input$period, layer_index)
      legend_title <- paste(info$label, time_label, sep = " - ")

      proxy |>
        addRasterImage(
          r,
          colors = pal,
          opacity = 0.75,
          layerId = "habitat_raster"
        ) |>
        addLegend(
          pal = pal,
          values = values(r),
          title = legend_title
        )
    })
  })
}
