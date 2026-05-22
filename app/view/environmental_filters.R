box::use(
  shiny[
    NS,
    moduleServer,
    tags,
    actionLink,
    checkboxGroupInput,
    reactive
  ],
  bslib[accordion, accordion_panel, popover],
  bsicons[bs_icon],
  app / view / habitat_suitability_sidebar[
    mod_habitat_suitability_sidebar_ui,
    mod_habitat_suitability_sidebar_server
  ],
  app / view / acoustic_telemetry_sidebar[
    mod_acoustic_telemetry_sidebar_ui,
    mod_acoustic_telemetry_sidebar_server
  ],
  app / view / passive_acoustic_monitoring_sidebar[
    mod_passive_acoustic_monitoring_sidebar_ui,
    mod_passive_acoustic_monitoring_sidebar_server
  ]
)

layer_info <- function(ns, id, label, description, href) {
  popover(
    actionLink(
      ns(id),
      label = "",
      icon = bs_icon(
        "info-circle",
        title = paste("More information about", label)
      )
    ),
    tags$p(class = "mb-2", description),
    tags$a(
      "Learn more",
      href = href,
      target = "_blank",
      rel = "noopener"
    ),
    title = label,
    placement = "right",
    options = list(container = "body")
  )
}

layer_choice <- function(ns, label, info_id, description, href) {
  tags$span(
    class = "d-inline-flex align-items-center gap-1",
    tags$span(label),
    layer_info(ns, info_id, label, description, href)
  )
}

mod_env_filters_ui <- function(id, wms_layers, bioflow_duc2_url) {
  ns <- NS(id)

  human_activity_layers <- list(
    list(
      label = "Offshore Wind Farms (OWF)",
      info_id = "owf_info",
      description = "Layer metadata and source details for offshore wind farms.",
      href = wms_layers$owf$wms_link[[1]]
    ),
    list(
      label = "Submarine Power Cables (SPC)",
      info_id = "spc_info",
      description = "Layer metadata and source details for submarine power cables.",
      href = wms_layers$spc$wms_link[[1]]
    ),
    list(
      label = "Marine Spatial Plans",
      info_id = "msp_info",
      description = "Layer metadata and source details for marine spatial planning zones.",
      href = wms_layers$msp$wms_link[[1]]
    ),
    list(
      label = "Sea convention polygons",
      info_id = "sea_conventions_info",
      description = "Layer metadata and source details for sea convention polygons.",
      href = wms_layers$sea_conventions$wms_link[[1]]
    ),
    list(
      label = "Shipwrecks",
      info_id = "shipwrecks_info",
      description = "Layer metadata and source details for shipwreck locations.",
      href = wms_layers$shipwrecks_emodnet$wms_link[[1]]
    )
  )

  natural_layers <- list(
    list(
      label = "Bathymetry (multicolor)",
      info_id = "bathy_multicolor_info",
      description = "Layer metadata and source details for multicolor bathymetry.",
      href = wms_layers$bathy_multicolor$wms_link[[1]]
    ),
    list(
      label = "Seabed habitats",
      info_id = "seabed_habitats_info",
      description = "Layer metadata and source details for seabed habitat classes.",
      href = wms_layers$seabedhabitats$wms_link[[1]]
    ),
    list(
      label = "Seabed substrates",
      info_id = "seabed_substrates_info",
      description = "Layer metadata and source details for seabed substrate classes.",
      href = wms_layers$seabedsubstrates$wms_link[[1]]
    )
  )

  choices <- function(layer_specs) {
    lapply(layer_specs, function(x) {
      layer_choice(
        ns = ns,
        label = x$label,
        info_id = x$info_id,
        description = x$description,
        href = x$href
      )
    })
  }

  values <- function(layer_specs) {
    vapply(layer_specs, function(x) x$label, character(1))
  }

  accordion(
    accordion_panel(
      "Human Activities layers", icon = bs_icon("building"),
      checkboxGroupInput(
        ns("human_activities_layers_sidebar"),
        NULL,
        choiceNames = choices(human_activity_layers),
        choiceValues = values(human_activity_layers)
      )
    ),
    accordion_panel(
      "Natural layers", icon = bs_icon("water"),
      checkboxGroupInput(
        ns("natural_layers_sidebar"),
        NULL,
        choiceNames = choices(natural_layers),
        choiceValues = values(natural_layers)
      )
    ),
    accordion_panel(
      "Habitat Suitability", icon = bs_icon("layers"),
      mod_habitat_suitability_sidebar_ui(ns("habitat_suitability"))
    ),
    accordion_panel(
      "Acoustic telemetry", icon = bs_icon("broadcast"),
      mod_acoustic_telemetry_sidebar_ui(
        ns("acoustic_telemetry"),
        show_raw_layer = TRUE
      )
    ),
    accordion_panel(
      "Passive acoustic monitoring", icon = bs_icon("soundwave"),
      mod_passive_acoustic_monitoring_sidebar_ui(ns("passive_acoustic_monitoring"))
    )
  )
}

mod_env_filters_server <- function(
  id,
  habitat_data = NULL,
  acoustic_months = NULL,
  acoustic_raw_months = NULL,
  pam_months = NULL
) {
  moduleServer(id, function(input, output, session) {
    habitat_selection <- mod_habitat_suitability_sidebar_server(
      "habitat_suitability",
      habitat_data = habitat_data
    )
    acoustic_selection <- mod_acoustic_telemetry_sidebar_server(
      "acoustic_telemetry",
      months = acoustic_months,
      raw_months = acoustic_raw_months,
      show_raw_layer = TRUE
    )
    passive_acoustic_monitoring_selection <-
      mod_passive_acoustic_monitoring_sidebar_server(
        "passive_acoustic_monitoring",
        months = pam_months
      )

    to_chr <- function(x) {
      if (is.null(x)) character(0) else x
    }

    active_habitat_layer <- function(habitat) {
      if (isTRUE(habitat$show_layer)) {
        return(habitat$layer_id)
      }

      character(0)
    }

    habitat_layer_for_species <- function(habitat, species) {
      if (isTRUE(habitat$show_layer) && identical(habitat$species, species)) {
        return(habitat$layer_id)
      }

      character(0)
    }

    reactive({
      habitat <- habitat_selection()
      acoustic <- acoustic_selection()
      passive_acoustic_monitoring <- passive_acoustic_monitoring_selection()

      list(
        human_activities = to_chr(input$human_activities_layers_sidebar),
        natural = to_chr(input$natural_layers_sidebar),
        habitat = habitat,
        acoustic_telemetry = acoustic,
        passive_acoustic_monitoring = passive_acoustic_monitoring,
        habitat_harbour_porpoise = habitat_layer_for_species(
          habitat,
          "harbour_porpoise"
        ),
        habitat_bottlenose_dolphin = habitat_layer_for_species(
          habitat,
          "bottlenose_dolphin"
        ),
        habitat_common_dolphin = habitat_layer_for_species(
          habitat,
          "common_dolphin"
        ),
        habitat_harbour_seal = habitat_layer_for_species(
          habitat,
          "harbour_seal"
        ),
        habitat_all = active_habitat_layer(habitat)
      )
    })
  })
}
