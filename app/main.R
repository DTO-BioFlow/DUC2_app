#import packages
box::use(
  shiny[
    NS,
    moduleServer,
    tagList,
    tags,
    a,
    actionLink,
    checkboxGroupInput
  ],
  bslib[
    page_navbar,
    nav_panel,
    accordion,
    accordion_panel,
    layout_sidebar,
    sidebar,
    navset_pill
  ],
  bsicons[bs_icon]
)
#import internal modules
box::use(
  app /
    logic /
    config[dto_colors, bioflow_url, bioflow_duc2_url, s3_bucket_habitatsuit_url],
  app / logic / maps[make_base_map, make_env_wms_map],
  app / logic / stac_data[load_STAC_metadata],
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
  app / view / lagrangian_connectivity[mod_lagrangian_connectivity_ui, mod_lagrangian_connectivity_server]
)

shiny::addResourcePath(
  "assets",
  normalizePath(file.path("app", "static"), mustWork = TRUE)
)

load(file.path("data", "DTO_DUC2_PpData.Rdata"))
TEL_deployments <- readRDS(file.path("data", "TEL_deployments.rds"))
TEL_detections <- readRDS(file.path("data", "TEL_detections.rds"))

wms_layers <- load_STAC_metadata(
  metadata_csv = file.path("data", "EDITO_STAC_layers_metadata.csv")
)
etn_monthyear_individual_sum <- build_monthyear_rds(
  output_path = "etn_sum_seabass_monthyear_individual.rds",
  wms_layer_metadata = wms_layers,
  dataset_key = "seabass acoustic detections"
)
habitat_suitability_s3 <- load_habitat_suitability_s3(
  s3_bucket_url = s3_bucket_habitatsuit_url
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  owf_info <- bslib::popover(
    actionLink(
      ns("owf_info"),
      label = bsicons::bs_icon("info-circle")
    ),
    "Here's a ",
    a("hyperlink", href = "https://google.com")
  )

  accordion_filters <- bslib::accordion(
    bslib::accordion_panel(
      "Human Activities layers", icon = bsicons::bs_icon("building"),
      checkboxGroupInput(
        ns("human_activities_layers_sidebar"),
        NULL,
        choiceNames = list(
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Offshore Wind Farms (OWF)"),
            owf_info
          ),
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Submarine Power Cables (SPC)"),
            bslib::popover(
              actionLink(
                ns("spc_info"),
                label = "",
                icon = bsicons::bs_icon(
                  "info-circle",
                  title = "More information about Submarine Power Cables (SPC)"
                )
              ),
              tags$p(
                class = "mb-2",
                "Layer metadata and source details for submarine power cables."
              ),
              tags$a(
                "Learn more",
                href = wms_layers$spc$wms_link[[1]],
                target = "_blank",
                rel = "noopener"
              ),
              title = "Submarine Power Cables (SPC)",
              placement = "right",
              options = list(container = "body")
            )
          ),
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Marine Spatial Plans"),
            bslib::popover(
              actionLink(
                ns("msp_info"),
                label = "",
                icon = bsicons::bs_icon(
                  "info-circle",
                  title = "More information about Marine Spatial Plans"
                )
              ),
              tags$p(
                class = "mb-2",
                "Layer metadata and source details for marine spatial planning zones."
              ),
              tags$a(
                "Learn more",
                href = wms_layers$msp$wms_link[[1]],
                target = "_blank",
                rel = "noopener"
              ),
              title = "Marine Spatial Plans",
              placement = "right",
              options = list(container = "body")
            )
          ),
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Sea convention polygons"),
            bslib::popover(
              actionLink(
                ns("sea_conventions_info"),
                label = "",
                icon = bsicons::bs_icon(
                  "info-circle",
                  title = "More information about Sea convention polygons"
                )
              ),
              tags$p(
                class = "mb-2",
                "Layer metadata and source details for sea convention polygons."
              ),
              tags$a(
                "Learn more",
                href = wms_layers$sea_conventions$wms_link[[1]],
                target = "_blank",
                rel = "noopener"
              ),
              title = "Sea convention polygons",
              placement = "right",
              options = list(container = "body")
            )
          ),
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Shipwrecks"),
            bslib::popover(
              actionLink(
                ns("shipwrecks_info"),
                label = "",
                icon = bsicons::bs_icon(
                  "info-circle",
                  title = "More information about Shipwrecks"
                )
              ),
              tags$p(
                class = "mb-2",
                "Layer metadata and source details for shipwreck locations."
              ),
              tags$a(
                "Learn more",
                href = wms_layers$shipwrecks_emodnet$wms_link[[1]],
                target = "_blank",
                rel = "noopener"
              ),
              title = "Shipwrecks",
              placement = "right",
              options = list(container = "body")
            )
          )
        ),
        choiceValues = c(
          "Offshore Wind Farms (OWF)",
          "Submarine Power Cables (SPC)",
          "Marine Spatial Plans",
          "Sea convention polygons",
          "Shipwrecks"
        )
      )
    ),
    bslib::accordion_panel(
      "Natural layers", icon = bsicons::bs_icon("water"),
      checkboxGroupInput(
        ns("natural_layers_sidebar"),
        NULL,
        choiceNames = list(
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Bathymetry (multicolor)"),
            bslib::popover(
              actionLink(
                ns("bathy_multicolor_info"),
                label = "",
                icon = bsicons::bs_icon(
                  "info-circle",
                  title = "More information about Bathymetry (multicolor)"
                )
              ),
              tags$p(
                class = "mb-2",
                "Layer metadata and source details for multicolor bathymetry."
              ),
              tags$a(
                "Learn more",
                href = wms_layers$bathy_multicolor$wms_link[[1]],
                target = "_blank",
                rel = "noopener"
              ),
              title = "Bathymetry (multicolor)",
              placement = "right",
              options = list(container = "body")
            )
          ),
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Seabed habitats"),
            bslib::popover(
              actionLink(
                ns("seabed_habitats_info"),
                label = "",
                icon = bsicons::bs_icon(
                  "info-circle",
                  title = "More information about Seabed habitats"
                )
              ),
              tags$p(
                class = "mb-2",
                "Layer metadata and source details for seabed habitat classes."
              ),
              tags$a(
                "Learn more",
                href = wms_layers$seabedhabitats$wms_link[[1]],
                target = "_blank",
                rel = "noopener"
              ),
              title = "Seabed habitats",
              placement = "right",
              options = list(container = "body")
            )
          ),
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Seabed substrates"),
            bslib::popover(
              actionLink(
                ns("seabed_substrates_info"),
                label = "",
                icon = bsicons::bs_icon(
                  "info-circle",
                  title = "More information about Seabed substrates"
                )
              ),
              tags$p(
                class = "mb-2",
                "Layer metadata and source details for seabed substrate classes."
              ),
              tags$a(
                "Learn more",
                href = wms_layers$seabedsubstrates$wms_link[[1]],
                target = "_blank",
                rel = "noopener"
              ),
              title = "Seabed substrates",
              placement = "right",
              options = list(container = "body")
            )
          )
        ),
        choiceValues = c(
          "Bathymetry (multicolor)",
          "Seabed habitats",
          "Seabed substrates"
        )
      )
    ),
    bslib::accordion_panel(
      "Habitat Suitability", icon = bsicons::bs_icon("layers"),
      bslib::accordion(
        bslib::accordion_panel(
          title = "harbour porpoise",
          value = "harbour_porpoise",
          checkboxGroupInput(
            ns("habitat_harbour_porpoise_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                bslib::popover(
                  actionLink(
                    ns("harbour_porpoise_present_monthly_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about harbour porpoise present monthly"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Monthly present-day habitat suitability output for harbour porpoise."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "harbour porpoise: present monthly",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present decade"),
                bslib::popover(
                  actionLink(
                    ns("harbour_porpoise_present_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about harbour porpoise present decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal present-day habitat suitability output for harbour porpoise."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "harbour porpoise: present decade",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("future decade"),
                bslib::popover(
                  actionLink(
                    ns("harbour_porpoise_future_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about harbour porpoise future decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal future habitat suitability output for harbour porpoise."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "harbour porpoise: future decade",
                  placement = "right",
                  options = list(container = "body")
                )
              )
            ),
            choiceValues = c(
              "harbour_porpoise_present_monthly",
              "harbour_porpoise_present_decade",
              "harbour_porpoise_future_decade"
            )
          )
        ),
        bslib::accordion_panel(
          title = "Bottlenose dolphin",
          value = "bottlenose_dolphin",
          checkboxGroupInput(
            ns("habitat_bottlenose_dolphin_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                bslib::popover(
                  actionLink(
                    ns("bottlenose_dolphin_present_monthly_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Bottlenose dolphin present monthly"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Monthly present-day habitat suitability output for Bottlenose dolphin."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Bottlenose dolphin: present monthly",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present decade"),
                bslib::popover(
                  actionLink(
                    ns("bottlenose_dolphin_present_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Bottlenose dolphin present decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal present-day habitat suitability output for Bottlenose dolphin."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Bottlenose dolphin: present decade",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("future decade"),
                bslib::popover(
                  actionLink(
                    ns("bottlenose_dolphin_future_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Bottlenose dolphin future decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal future habitat suitability output for Bottlenose dolphin."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Bottlenose dolphin: future decade",
                  placement = "right",
                  options = list(container = "body")
                )
              )
            ),
            choiceValues = c(
              "bottlenose_dolphin_present_monthly",
              "bottlenose_dolphin_present_decade",
              "bottlenose_dolphin_future_decade"
            )
          )
        ),
        bslib::accordion_panel(
          title = "Common dolphin",
          value = "common_dolphin",
          checkboxGroupInput(
            ns("habitat_common_dolphin_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                bslib::popover(
                  actionLink(
                    ns("common_dolphin_present_monthly_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Common dolphin present monthly"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Monthly present-day habitat suitability output for Common dolphin."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Common dolphin: present monthly",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present decade"),
                bslib::popover(
                  actionLink(
                    ns("common_dolphin_present_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Common dolphin present decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal present-day habitat suitability output for Common dolphin."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Common dolphin: present decade",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("future decade"),
                bslib::popover(
                  actionLink(
                    ns("common_dolphin_future_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Common dolphin future decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal future habitat suitability output for Common dolphin."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Common dolphin: future decade",
                  placement = "right",
                  options = list(container = "body")
                )
              )
            ),
            choiceValues = c(
              "common_dolphin_present_monthly",
              "common_dolphin_present_decade",
              "common_dolphin_future_decade"
            )
          )
        ),
        bslib::accordion_panel(
          title = "Harbour Seal",
          value = "harbour_seal",
          checkboxGroupInput(
            ns("habitat_harbour_seal_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                bslib::popover(
                  actionLink(
                    ns("harbour_seal_present_monthly_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Harbour Seal present monthly"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Monthly present-day habitat suitability output for Harbour Seal."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Harbour Seal: present monthly",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present decade"),
                bslib::popover(
                  actionLink(
                    ns("harbour_seal_present_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Harbour Seal present decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal present-day habitat suitability output for Harbour Seal."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Harbour Seal: present decade",
                  placement = "right",
                  options = list(container = "body")
                )
              ),
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("future decade"),
                bslib::popover(
                  actionLink(
                    ns("harbour_seal_future_decade_info"),
                    label = "",
                    icon = bsicons::bs_icon(
                      "info-circle",
                      title = "More information about Harbour Seal future decade"
                    )
                  ),
                  tags$p(
                    class = "mb-2",
                    "Decadal future habitat suitability output for Harbour Seal."
                  ),
                  tags$a(
                    "Learn more",
                    href = bioflow_duc2_url,
                    target = "_blank",
                    rel = "noopener"
                  ),
                  title = "Harbour Seal: future decade",
                  placement = "right",
                  options = list(container = "body")
                )
              )
            ),
            choiceValues = c(
              "harbour_seal_present_monthly",
              "harbour_seal_present_decade",
              "harbour_seal_future_decade"
            )
          )
        )
      )
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
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 320,
          open = "desktop",
          title = "Layers",
          accordion_filters
        ),
        mod_env_ui(
          ns("env"),
          base_map_fun = make_base_map,
          make_env_wms_map_fun = make_env_wms_map,
          wms_layers = wms_layers
        )
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
    mod_home_server("home")
    mod_seabass_server(
      "seabass",
      TEL_deployments = TEL_deployments,
      TEL_detections = TEL_detections,
      etn_monthyear_individual_sum = etn_monthyear_individual_sum,
      base_map_fun = make_base_map,
      prep_minicharts_inputs_fun = prep_minicharts_inputs
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
      habitat_data = habitat_suitability_s3
    )
    mod_lagrangian_connectivity_server(
      id = "lagrangian_connectivity",
      base_map_fun = make_base_map
    )
  })
}
