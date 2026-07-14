box::use(
  shiny[
    NS,
    moduleServer,
    tagList,
    tags,
    a,
    actionLink,
    checkboxGroupInput,
    
    addResourcePath,
    reactive
  ],
  bslib[
    page_navbar,
    nav_panel,
    navbar_options,
    navset_pill,
  ],
  htmltools[HTML],
  glue[glue],
)
#import internal modules
box::use(
  app /
    logic /
    config[dto_colors, bioflow_url, bioflow_duc2_url, s3_bucket_habitatsuit_url],
  app / logic / maps[make_base_map, make_env_wms_map],
  app / logic / stac_data[load_STAC_metadata],
  app / logic / SB4S / SB4S_api[load_SB4S_simout],
  app /
    logic /
    hsuit /
    habitat_suitability_s3[load_habitat_suitability_s3],
  app /
    logic /
    seabass /
    telemetry_wrangle[build_monthyear_rds, prep_minicharts_inputs],
  app / view / home[mod_home_ui, mod_home_server],
  app / view / seabass / main[mod_seabass_ui, mod_seabass_server],
  app / view / porpoise[mod_porpoise_ui, mod_porpoise_server],
  app / view / environmental_data[mod_env_ui, mod_env_server],
  app / view / habitat_suitability[mod_habitat_suitability_ui, mod_habitat_suitability_server],
  app / view / SB4S[mod_SB4S_ui, mod_SB4S_server]
)

addResourcePath(
  "assets",
  normalizePath(box::file("static"), mustWork = TRUE)
)

load(box::file("..", "data", "DTO_DUC2_PpData.Rdata"))
TEL_deployments <- readRDS(box::file("..", "data", "TEL_deployments.rds"))
TEL_detections <- readRDS(box::file("..", "data", "TEL_detections.rds"))

wms_layers <- load_STAC_metadata(
  metadata_csv = box::file("..", "data", "EDITO_STAC_layers_metadata.csv")
)
etn_monthyear_individual_sum <- build_monthyear_rds(
  output_path = box::file(
    "..",
    "data",
    "etn_sum_seabass_monthyear_individual.rds"
  ),
  wms_layer_metadata = wms_layers,
  dataset_key = "seabass acoustic detections"
)
habitat_suitability_s3 <- load_habitat_suitability_s3(
  s3_bucket_url = s3_bucket_habitatsuit_url
)

SB4S_frozensim <- load_SB4S_simout(box::file("..", "data", "sb4s_simout_52w.nc"))

#' @export
ui <- function(id) {
  ns <- NS(id)
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
      tags$style(HTML(glue(
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
    window_title = "Marine life habitat use in potential offshore infrastructure areas",
    navbar_options = navbar_options(
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
            "SB4S seabass simulation",
            style = "font-size: 16px; vertical-align:middle;"
          ),
          mod_SB4S_ui(ns("SB4S"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mod_home_server("home")
    mod_seabass_server(
      "seabass",
      TEL_deployments = TEL_deployments,
      TEL_detections = TEL_detections,
      etn_monthyear_individual_sum = etn_monthyear_individual_sum,
      prep_minicharts_inputs_fun = prep_minicharts_inputs
    )
    mod_porpoise_server(
      id = "porpoise",
      SCANS_shape = SCANS_shape,
      POD_loc_sf = POD_loc_sf,
      PAM_data = PAM_data,
      PAM_grd = PAM_grd,
      POD_locations = POD_locations
    )
    env_layers <- mod_env_server(
      id = "env",
      wms_layers = wms_layers,
      habitat_data = habitat_suitability_s3,
      acoustic_detections = TEL_detections,
      pam_data = PAM_data
    )
    mod_habitat_suitability_server(
      id = "habitat_suitability",
      sidebar_layers = reactive(env_layers()$habitat_all),
      habitat_data = habitat_suitability_s3
    )
    mod_SB4S_server(
      id = "SB4S",
      biodata = SB4S_frozensim
    )
  })
}
