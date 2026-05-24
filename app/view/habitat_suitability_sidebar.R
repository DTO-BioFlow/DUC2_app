box::use(
  shiny[
    NS,
    moduleServer,
    radioButtons,
    selectInput,
    sliderInput,
    textOutput,
    uiOutput,
    renderUI,
    renderText,
    tags,
    observe,
    reactive,
    req,
    updateSliderInput,
    isolate
  ],
  bslib[input_switch],
  terra[nlyr]
)

habitat_period_choices <- function() {
  c(
    "Monthly" = "present_monthly",
    "Decadal (2000-2019)" = "present_decade",
    "Decadal climate scenarios (2020-2100)" = "future_decade"
  )
}

habitat_species_choices <- function() {
  c(
    "Harbour porpoise" = "harbour_porpoise",
    "Bottlenose dolphin" = "bottlenose_dolphin",
    "Common dolphin" = "common_dolphin",
    "Harbour seal" = "harbour_seal"
  )
}

habitat_scenario_choices <- function() {
  c(
    "SSP1-1.9" = "ssp119",
    "SSP1-2.6" = "ssp126",
    "SSP2-4.5" = "ssp245",
    "SSP3-7.0" = "ssp370",
    "SSP4-6.0" = "ssp460",
    "SSP5-8.5" = "ssp585"
  )
}

habitat_present_decade_labels <- c("2000-2009", "2010-2019")
habitat_future_decade_labels <- c(
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

habitat_layer_id <- function(species, period, scenario = NULL) {
  if (identical(period, "future_decade")) {
    if (
      is.null(scenario) ||
        length(scenario) != 1 ||
        is.na(scenario) ||
        !nzchar(scenario)
    ) {
      scenario <- "ssp119"
    }

    return(paste(species, period, scenario, sep = "_"))
  }

  paste(species, period, sep = "_")
}

habitat_fallback_layer_count <- function(period) {
  if (period == "present_monthly") {
    return(12)
  }

  if (period == "present_decade") {
    return(length(habitat_present_decade_labels))
  }

  length(habitat_future_decade_labels)
}

habitat_time_label <- function(r_stack, period, index) {
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

  if (
    period == "present_decade" &&
      index <= length(habitat_present_decade_labels)
  ) {
    return(habitat_present_decade_labels[index])
  }

  if (
    period == "future_decade" &&
      index <= length(habitat_future_decade_labels)
  ) {
    return(habitat_future_decade_labels[index])
  }

  paste("Layer", index)
}

mod_habitat_suitability_sidebar_ui <- function(id, show_toggle = TRUE) {
  ns <- NS(id)

  tags$div(
    class = "habitat-suitability-sidebar",
    if (show_toggle) {
      input_switch(
        ns("show_layer"),
        "Show habitat suitability layer",
        value = FALSE
      )
    },
    radioButtons(
      ns("period"),
      "Layer",
      choices = habitat_period_choices(),
      selected = "present_monthly"
    ),
    selectInput(
      ns("species"),
      "Species",
      choices = habitat_species_choices(),
      selected = "harbour_porpoise"
    ),
    uiOutput(ns("scenario_picker")),
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
  )
}

mod_habitat_suitability_sidebar_server <- function(
  id,
  habitat_data = NULL,
  show_toggle = TRUE
) {
  moduleServer(id, function(input, output, session) {
    output$scenario_picker <- renderUI({
      req(input$period)

      if (input$period != "future_decade") {
        return(NULL)
      }

      selectInput(
        session$ns("scenario"),
        "Climate scenario",
        choices = habitat_scenario_choices(),
        selected = "ssp119"
      )
    })

    selected_scenario <- reactive({
      req(input$period)

      if (input$period != "future_decade") {
        return("present")
      }

      if (is.null(input$scenario)) {
        return("ssp119")
      }

      input$scenario
    })

    selected_layer_id <- reactive({
      req(input$species, input$period)
      habitat_layer_id(input$species, input$period, selected_scenario())
    })

    current_raster_stack <- reactive({
      req(selected_layer_id())

      if (is.null(habitat_data)) {
        return(NULL)
      }

      habitat_data$habitat_layers[[selected_layer_id()]]
    })

    current_layer_count <- reactive({
      req(input$period)
      layer_info <- if (is.null(habitat_data)) {
        NULL
      } else {
        habitat_data$habitat_layers_info[[selected_layer_id()]]
      }

      if (!is.null(layer_info$layer_count)) {
        return(layer_info$layer_count)
      }

      r_stack <- current_raster_stack()
      if (is.null(r_stack)) {
        return(habitat_fallback_layer_count(input$period))
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

    selected_time_label <- reactive({
      req(input$period, input$time_index)
      habitat_time_label(
        r_stack = current_raster_stack(),
        period = input$period,
        index = input$time_index
      )
    })

    output$time_label <- renderText({
      selected_time_label()
    })

    reactive({
      req(input$species, input$period, input$time_index)

      list(
        show_layer = if (show_toggle) isTRUE(input$show_layer) else TRUE,
        species = input$species,
        period = input$period,
        scenario = selected_scenario(),
        layer_id = selected_layer_id(),
        time_index = input$time_index,
        time_label = selected_time_label()
      )
    })
  })
}

habitat_suitability_selected_layer <- function(habitat_data, selection) {
  if (
    is.null(habitat_data) ||
      is.null(selection) ||
      !isTRUE(selection$show_layer)
  ) {
    return(NULL)
  }

  layer_id <- selection$layer_id
  time_index <- suppressWarnings(as.integer(selection$time_index))

  if (
    is.null(layer_id) ||
      length(layer_id) != 1 ||
      is.na(layer_id) ||
      !nzchar(layer_id) ||
      length(time_index) != 1 ||
      is.na(time_index)
  ) {
    return(NULL)
  }

  layer <- if (!is.null(habitat_data$get_layer)) {
    habitat_data$get_layer(layer_id)
  } else {
    list(
      raster = habitat_data$habitat_layers[[layer_id]],
      palette = habitat_data$habitat_palettes[[layer_id]],
      domain = c(0, 1)
    )
  }

  if (is.null(layer) || is.null(layer$raster) || is.null(layer$palette)) {
    return(NULL)
  }

  r_stack <- layer$raster
  pal <- layer$palette
  layer_index <- min(max(1, time_index), nlyr(r_stack))
  info <- habitat_data$habitat_layers_info[[layer_id]]
  layer_label <- if (!is.null(info$label)) info$label else layer_id
  time_label <- selection$time_label

  if (
    is.null(time_label) ||
      length(time_label) != 1 ||
      !nzchar(time_label)
  ) {
    time_label <- habitat_time_label(r_stack, selection$period, layer_index)
  }

  list(
    raster = r_stack[[layer_index]],
    palette = pal,
    domain = layer$domain,
    title = paste(layer_label, time_label, sep = " - ")
  )
}
