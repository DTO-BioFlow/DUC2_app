##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-02-03
# Script Name: ~/DUC2_viewer_acoustic_telemetry/R/module_seabass_telemetry.R
# Script Description: make the seabass tab, containing acoustic telemetry data

##################################################################################
##################################################################################

box::use(
  shiny[NS, moduleServer],
  bslib[navset_card_tab, nav_panel, page_fluid],
  app /
    view /
    seabass /
    telemetry_data[
      mod_seabass_telemetry_ui,
      mod_seabass_telemetry_data_server
    ],
  app /
    view /
    seabass /
    network_analysis[
      mod_seabass_network_analysis_ui,
      mod_seabass_network_analysis_server
    ]
)

mod_seabass_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    navset_card_tab(
      nav_panel(
        "Raw data",
        mod_seabass_telemetry_ui(ns("telemetry_data"))
      ),
      nav_panel(
        "Network analysis",
        mod_seabass_network_analysis_ui(ns("network_analysis"))
      )
    )
    # nav_panel("Environmental layers", mod_seabass_env_ui(ns("env")))
  )
}

mod_seabass_server <- function(
  id,
  TEL_deployments,
  TEL_detections,
  etn_monthyear_individual_sum,
  base_map_fun,
  prep_minicharts_inputs_fun
) {
  moduleServer(id, function(input, output, session) {
    # Prepare data for the telemetry bubble map once.
    prepped_data <- prep_minicharts_inputs_fun(
      TEL_deployments,
      etn_monthyear_individual_sum
    )

    # Telemetry submodule
    mod_seabass_telemetry_data_server(
      "telemetry_data",
      prepped_data = prepped_data,
      etn_monthyear_individual_sum = etn_monthyear_individual_sum,
      base_map_fun = base_map_fun
    )

    mod_seabass_network_analysis_server(
      "network_analysis",
      detections = TEL_detections,
      base_map_fun = base_map_fun
    )

    # # Environmental submodule
    # mod_seabass_env_server(
    #   "env",
    #   wms_layers = wms_layers,         # Use parameter name
    #   base_map_fun = base_map_fun,     # Use parameter name
    #   env_map_fun = make_env_wms_map_fun  # Use parameter name
    # )
  })
}
