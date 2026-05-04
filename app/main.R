#import packages
box::use(
  shiny[
    NS,
    moduleServer,
    tagList,
    tags,
    checkboxGroupInput,
    reactive
  ],
  bslib[
    page_navbar,
    nav_panel,
    accordion,
    accordion_panel,
    sidebar,
    navset_pill
  ],
  bsicons[bs_icon]
)
#import internal modules
box::use(
  app /
    logic /
    config[dto_colors, bioflow_url, bioflow_duc2_url, s3_bucket_seabass_url],
  app / logic / maps[make_base_map, make_env_wms_map],
  app / logic / stac_data[load_STAC_metadata],
  app / logic / seabass / gam_s3[load_acoustic_telemetry_GAM_s3],
  app /
    logic /
    seabass /
    telemetry_wrangle[build_monthyear_rds, prep_minicharts_inputs],
  app / view / home[mod_home_ui, mod_home_server],
  app / view / seabass / main[mod_seabass_ui, mod_seabass_server],
  app / view / porpoise[mod_porpoise_ui, mod_porpoise_server],
  app / view / environmental_data[mod_env_ui, mod_env_server],
  app / view / habitat_suitability[mod_habitat_suitability_ui, mod_habitat_suitability_server],
  app / view / lagrangian_connectivity[mod_lagrangian_connectivity_ui, mod_lagrangian_connectivity_server]
)

shiny::addResourcePath(
  "assets",
  normalizePath(file.path("app", "static"), mustWork = TRUE)
)

load(file.path("data", "DTO_DUC2_PpData.Rdata"))
TEL_deployments <- readRDS(file.path("data", "TEL_deployments.rds"))

wms_layers <- load_STAC_metadata(
  metadata_csv = file.path("data", "EDITO_STAC_layers_metadata.csv")
)
telemetry_gam_s3 <- load_acoustic_telemetry_GAM_s3(
  s3_bucket_url = s3_bucket_seabass_url
)
etn_monthyear_individual_sum <- build_monthyear_rds(
  output_path = "etn_sum_seabass_monthyear_individual.rds",
  wms_layer_metadata = wms_layers,
  dataset_key = "seabass acoustic detections"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  accordion_filters <- bslib::accordion(
    bslib::accordion_panel(
      "Map layers", icon = bsicons::bs_icon("menu-app"),
      checkboxGroupInput(
        "layers",
        "Map layers",
        choices = c(
          "Bathymetry (multicolor)",
          "Seabed substrates",
          "Seabed habitats",
          "Marine Spatial Plans"
        ),
        selected = "Bathymetry (multicolor)"
      )
    ),
    bslib::accordion_panel(
      "Habitat Suitability", icon = bsicons::bs_icon("layers"),
      checkboxGroupInput(
        ns("habitat_layers_sidebar"),
        "Habitat Suitability Layers",
        choices = c(
          "Layer 1" = "layer_1",
          "Layer 2" = "layer_2",
          "Layer 3" = "layer_3"
        ),
        selected = "layer_1"
      )
    ),
    bslib::accordion_panel(
      "Lagrangian Connectivity", icon = bsicons::bs_icon("diagram-3"),
      checkboxGroupInput(
        ns("connectivity_layers_sidebar"),
        "Connectivity Layers",
        choices = c(
          "Connectivity plot" = "connectivity"
        ),
        selected = "connectivity"
      )
    ),
    bslib::accordion_panel(
      "Numerical", icon = bsicons::bs_icon("sliders"),
      "Placeholder for numerical filters"
    )
  )
  page_navbar(
    title = tagList(
      tags$a(
        href = bioflow_url,
        target = "_blank",
        rel = "noopener",
        class = "navbar-logo-link",
        tags$img(
          src = "assets/Logo_BIO-Flow2023_Final_Positive.png",
          height = "42px",
          alt = "DTO-Bioflow"
        )
      ),
      tags$span(
        class = "navbar-page-title",
        "Marine life habitat use in potential offshore infrastructure areas"
      )
    ),
    id = ns("tabsetPanelID"),
    header = tags$head(
      tags$style(htmltools::HTML(glue::glue(
        "\
      :root {{
        --blue-light: {dto_colors$blue_light};
        --blue-medium: {dto_colors$blue_medium};
        --blue-dark: {dto_colors$blue_dark};
      }}
    "
      ))),
      tags$link(rel = "stylesheet", type = "text/css", href = "assets/css/app.min.css")
    ),
    sidebar = bslib::sidebar(
      width = 320,
      open = "desktop",
      title = "Global sidebar",
      accordion_filters
    ),
    window_title = "Marine life habitat use in potential offshore infrastructure areas",
    navbar_options = bslib::navbar_options(
      bg = "var(--blue-light)",
      theme = "light",
      underline = TRUE
    ),
    nav_panel(
      "Home",
      mod_home_ui(
        ns("home"),
        bioflow_url = bioflow_url,
        bioflow_duc2_url = bioflow_duc2_url,
        colors = dto_colors
      )
    ),
    nav_panel(
      title = tags$span(
        "Overview",
        style = "font-size: 16px; vertical-align:middle;"
      ),
      class = "lower-level-tabs",
      mod_env_ui(
        ns("env"),
        base_map_fun = make_base_map,
        make_env_wms_map_fun = make_env_wms_map,
        wms_layers = wms_layers
      )
    ),
    nav_panel(
      "Datatypes",
      navset_pill(
        id = ns("datatypes"),
        nav_panel(
          title = tags$span(
            "Acoustic telemetry",
            style = "font-size: 16px; vertical-align:middle;"
          ),
          mod_seabass_ui(ns("seabass"))
        ),
        nav_panel(
          title = tags$span(
            "Passive acoustic monitoring",
            style = "font-size: 16px; vertical-align:middle;"
          ),
          mod_porpoise_ui(ns("porpoise"))
        ),
        nav_panel(
          title = tags$span(
            "Habitat suitability",
            style = "font-size: 16px; vertical-align:middle;"
          ),
          mod_habitat_suitability_ui(ns("habitat_suitability"))
        ),
        nav_panel(
          title = tags$span(
            "Lagrangian connectivity",
            style = "font-size: 16px; vertical-align:middle;"
          ),
          mod_lagrangian_connectivity_ui(ns("lagrangian_connectivity"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Create reactive expressions for sidebar layer selections
    habitat_layers_reactive <- reactive({
      input$habitat_layers_sidebar
    })
    
    connectivity_layers_reactive <- reactive({
      input$connectivity_layers_sidebar
    })
    
    mod_home_server("home")
    mod_seabass_server(
      "seabass",
      TEL_deployments = TEL_deployments,
      etn_monthyear_individual_sum = etn_monthyear_individual_sum,
      base_map_fun = make_base_map,
      prep_minicharts_inputs_fun = prep_minicharts_inputs,
      make_env_wms_map_fun = make_env_wms_map,
      telemetry_gam_s3 = telemetry_gam_s3,
      wms_layers = wms_layers
    )
    mod_porpoise_server(
      id = "porpoise",
      SCANS_shape = SCANS_shape,
      POD_loc_sf = POD_loc_sf,
      PAM_data = PAM_data,
      PAM_grd = PAM_grd,
      POD_locations = POD_locations,
      base_map_fun = make_base_map
    )
    mod_env_server(
      id = "env",
      wms_layers = wms_layers,
      base_map_fun = make_base_map,
      make_env_wms_map_fun = make_env_wms_map
    )
    mod_habitat_suitability_server(
      id = "habitat_suitability",
      base_map_fun = make_base_map,
      sidebar_layers = habitat_layers_reactive
    )
    mod_lagrangian_connectivity_server(
      id = "lagrangian_connectivity",
      base_map_fun = make_base_map,
      sidebar_layers = connectivity_layers_reactive
    )
  })
}
