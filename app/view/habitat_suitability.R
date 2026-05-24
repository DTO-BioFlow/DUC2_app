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
    observe
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
  ]
)

# ui ----------------------------------------------------------------------

mod_habitat_suitability_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      open = "open",
      title = "Habitat suitability",
      mod_habitat_suitability_sidebar_ui(ns("controls"))
    ),
    leafletOutput(ns("habitat_map"), height = 700)
  )
}

box::use(
  app / logic / maps[make_base_map],
  app / view / habitat_suitability_sidebar[
    mod_habitat_suitability_sidebar_ui,
    mod_habitat_suitability_sidebar_server,
    habitat_suitability_selected_layer
  ]
)
# server  -----------------------------------------------------------------

mod_habitat_suitability_server <- function(
  id,
  habitat_data = NULL,
  sidebar_layers = NULL
) {
  moduleServer(id, function(input, output, session) {
    habitat_selection <- mod_habitat_suitability_sidebar_server(
      "controls",
      habitat_data = habitat_data
    )

    # Initialize base map
    output$habitat_map <- renderLeaflet({
      make_base_map() |>
        setView(lat = 51.5, lng = 2.5, zoom = 8)
    })

    observe({
      map_hidden <- session$clientData[[
        paste0("output_", session$ns("habitat_map"), "_hidden")
      ]]
      if (!isFALSE(map_hidden)) {
        return(invisible(NULL))
      }

      proxy <- leafletProxy("habitat_map", session = session) |>
        clearImages() |>
        clearControls()

      layer <- habitat_suitability_selected_layer(
        habitat_data = habitat_data,
        selection = habitat_selection()
      )

      if (is.null(layer)) {
        return(invisible(NULL))
      }

      proxy |>
        addRasterImage(
          layer$raster,
          colors = layer$palette,
          opacity = 0.75,
          layerId = "habitat_raster"
        ) |>
        addLegend(
          pal = layer$palette,
          values = layer$domain,
          title = layer$title
        )
    })
  })
}
