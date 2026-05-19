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
    br,
    tags,
    observe,
    reactive,
    req,
    observeEvent
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
  terra[values]
)

# ui ----------------------------------------------------------------------

mod_habitat_suitability_ui <- function(id) {
  ns <- NS(id)

  leafletOutput(ns("habitat_map"), height = 700)
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
    
    # Initialize base map
    output$habitat_map <- renderLeaflet({
      make_base_map() |>
        setView(lat = 51.5, lng = 2.5, zoom = 8)
    })

    # Observer to update layers based on sidebar selection
    observe({
      # If sidebar_layers reactive is provided, use it
      if (!is.null(sidebar_layers)) {
        selected_layers <- sidebar_layers()
      } else {
        # Fallback if not provided
        selected_layers <- c()
      }
      
      # Update map with selected layers
      leafletProxy("habitat_map", session = session) |>
        clearImages() |>
        clearControls()
      
      # Add each selected layer to the map
      # NOTE: Update this section when habitat_data is provided
      # Example of how to add layers:
      # if ("layer_1" %in% selected_layers && !is.null(habitat_data$layer_1)) {
      #   leafletProxy("habitat_map", session = session) |>
      #     addRasterImage(
      #       habitat_data$layer_1,
      #       colors = habitat_data$pal_layer_1,
      #       opacity = 0.7,
      #       layerId = "layer_1"
      #     ) |>
      #     addLegend(
      #       pal = habitat_data$pal_layer_1,
      #       values = values(habitat_data$layer_1),
      #       title = "Layer 1"
      #     )
      # }
    })
  })
}
