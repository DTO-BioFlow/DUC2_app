##################################################################################
##################################################################################

# Author: Assistant
# Date: 2026-05-04
# Script Name: ~/DUC2_app/app/view/lagrangian_connectivity.R
# Script Description: Lagrangian connectivity raster display with layer toggles

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

mod_lagrangian_connectivity_ui <- function(id) {
  ns <- NS(id)

  leafletOutput(ns("connectivity_map"), height = 700)
}


# server  -----------------------------------------------------------------

mod_lagrangian_connectivity_server <- function(
  id,
  base_map_fun,
  connectivity_data = NULL,
  sidebar_layers = NULL
) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize base map
    output$connectivity_map <- renderLeaflet({
      req(base_map_fun)
      base_map_fun() |>
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
      leafletProxy("connectivity_map", session = session) |>
        clearImages() |>
        clearControls()
      
      # Add each selected layer to the map
      # NOTE: Update this section when connectivity_data is provided
      # Example of how to add layers:
      # if ("release_sites" %in% selected_layers && !is.null(connectivity_data$release_sites)) {
      #   leafletProxy("connectivity_map", session = session) |>
      #     addRasterImage(
      #       connectivity_data$release_sites,
      #       colors = connectivity_data$pal_release,
      #       opacity = 0.7,
      #       layerId = "release_sites"
      #     ) |>
      #     addLegend(
      #       pal = connectivity_data$pal_release,
      #       values = values(connectivity_data$release_sites),
      #       title = "Release sites"
      #     )
      # }
    })
  })
}
