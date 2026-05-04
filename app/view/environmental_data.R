box::use(
  shiny[NS, moduleServer],
  leaflet[leafletOutput, renderLeaflet]
)

mod_env_ui <- function(id, base_map_fun, make_env_wms_map_fun, wms_layers) {
  ns <- NS(id)
  leafletOutput(ns("env_map"), height = 600)
}


mod_env_server <- function(id, base_map_fun, make_env_wms_map_fun, wms_layers) {
  moduleServer(id, function(input, output, session) {
    output$env_map <- renderLeaflet({
      make_env_wms_map_fun(base_map = base_map_fun(), wms_layers = wms_layers)
    })
  })
}