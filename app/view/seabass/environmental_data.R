box::use(
  shiny[NS, moduleServer],
  leaflet[leafletOutput, renderLeaflet]
)

mod_seabass_env_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("env_map"), height = 600)
}

mod_seabass_env_server <- function(id, wms_layers, env_map_fun) {
  moduleServer(id, function(input, output, session) {
    output$env_map <- renderLeaflet({
      env_map_fun(base_map = make_base_map, wms_layers = wms_layers)
    })
  })
}
