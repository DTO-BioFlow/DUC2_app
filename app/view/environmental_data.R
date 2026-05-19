box::use(
  shiny[NS, moduleServer, observeEvent, reactiveVal],
  leaflet[leafletOutput, renderLeaflet],
  bslib[layout_sidebar, sidebar],
  app / view / environmental_filters[mod_env_filters_ui, mod_env_filters_server],
  app / logic / config[bioflow_duc2_url]
)

box::use(
  app / logic / maps[make_base_map, make_env_base_map, update_env_wms_map],
)
mod_env_ui <- function(id, wms_layers) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      open = "desktop",
      title = "Layers",
      mod_env_filters_ui(
        ns("filters"),
        wms_layers = wms_layers,
        bioflow_duc2_url = bioflow_duc2_url
      )
    ),
    leafletOutput(ns("env_map"), height = 600)
  )
}


mod_env_server <- function(id, wms_layers) {
  moduleServer(id, function(input, output, session) {
    selected_layers <- mod_env_filters_server("filters")
    active_layers <- reactiveVal(character(0))

    output$env_map <- renderLeaflet({
      make_env_base_map(base_map = make_base_map, wms_layers = wms_layers)
    })

    observeEvent(selected_layers(),
      {
        current_layers <- update_env_wms_map(
          map_id = "env_map",
          session = session,
          wms_layers = wms_layers,
          selected_layers = selected_layers(),
          cached_layers = active_layers()
        )

        active_layers(current_layers)
      },
      ignoreNULL = FALSE
    )

    selected_layers
  })
}
