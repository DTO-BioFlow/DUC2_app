box::use(
  shiny[NS, moduleServer, tags, a, actionLink, checkboxGroupInput, reactive],
  bslib[accordion, accordion_panel, popover],
  bsicons[bs_icon]
)

mod_env_filters_ui <- function(id, wms_layers, bioflow_duc2_url) {
  ns <- NS(id)
  owf_info <- popover(
    actionLink(
      ns("owf_info"),
      label = bs_icon("info-circle")
    ),
    "Here's a ",
    a("hyperlink", href = "https://google.com")
  )

  accordion_filters <- accordion(
    accordion_panel(
      "Human Activities layers", icon = bs_icon("building"),
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
            popover(
              actionLink(
                ns("spc_info"),
                label = "",
                icon = bs_icon(
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
            popover(
              actionLink(
                ns("msp_info"),
                label = "",
                icon = bs_icon(
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
            popover(
              actionLink(
                ns("sea_conventions_info"),
                label = "",
                icon = bs_icon(
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
            popover(
              actionLink(
                ns("shipwrecks_info"),
                label = "",
                icon = bs_icon(
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
    accordion_panel(
      "Natural layers", icon = bs_icon("water"),
      checkboxGroupInput(
        ns("natural_layers_sidebar"),
        NULL,
        choiceNames = list(
          tags$span(
            class = "d-inline-flex align-items-center gap-1",
            tags$span("Bathymetry (multicolor)"),
            popover(
              actionLink(
                ns("bathy_multicolor_info"),
                label = "",
                icon = bs_icon(
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
            popover(
              actionLink(
                ns("seabed_habitats_info"),
                label = "",
                icon = bs_icon(
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
            popover(
              actionLink(
                ns("seabed_substrates_info"),
                label = "",
                icon = bs_icon(
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
    accordion_panel(
      "Habitat Suitability", icon = bs_icon("layers"),
      accordion(
        accordion_panel(
          title = "harbour porpoise",
          value = "harbour_porpoise",
          checkboxGroupInput(
            ns("habitat_harbour_porpoise_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                popover(
                  actionLink(
                    ns("harbour_porpoise_present_monthly_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("harbour_porpoise_present_decade_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("harbour_porpoise_future_decade_info"),
                    label = "",
                    icon = bs_icon(
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
        accordion_panel(
          title = "Bottlenose dolphin",
          value = "bottlenose_dolphin",
          checkboxGroupInput(
            ns("habitat_bottlenose_dolphin_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                popover(
                  actionLink(
                    ns("bottlenose_dolphin_present_monthly_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("bottlenose_dolphin_present_decade_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("bottlenose_dolphin_future_decade_info"),
                    label = "",
                    icon = bs_icon(
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
        accordion_panel(
          title = "Common dolphin",
          value = "common_dolphin",
          checkboxGroupInput(
            ns("habitat_common_dolphin_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                popover(
                  actionLink(
                    ns("common_dolphin_present_monthly_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("common_dolphin_present_decade_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("common_dolphin_future_decade_info"),
                    label = "",
                    icon = bs_icon(
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
        accordion_panel(
          title = "Harbour Seal",
          value = "harbour_seal",
          checkboxGroupInput(
            ns("habitat_harbour_seal_sidebar"),
            NULL,
            choiceNames = list(
              tags$span(
                class = "d-inline-flex align-items-center gap-1",
                tags$span("present monthly"),
                popover(
                  actionLink(
                    ns("harbour_seal_present_monthly_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("harbour_seal_present_decade_info"),
                    label = "",
                    icon = bs_icon(
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
                popover(
                  actionLink(
                    ns("harbour_seal_future_decade_info"),
                    label = "",
                    icon = bs_icon(
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

  accordion_filters
}

mod_env_filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    to_chr <- function(x) {
      if (is.null(x)) character(0) else x
    }

    reactive({
      habitat_harbour_porpoise <- to_chr(input$habitat_harbour_porpoise_sidebar)
      habitat_bottlenose_dolphin <- to_chr(input$habitat_bottlenose_dolphin_sidebar)
      habitat_common_dolphin <- to_chr(input$habitat_common_dolphin_sidebar)
      habitat_harbour_seal <- to_chr(input$habitat_harbour_seal_sidebar)

      list(
        human_activities = to_chr(input$human_activities_layers_sidebar),
        natural = to_chr(input$natural_layers_sidebar),
        habitat_harbour_porpoise = habitat_harbour_porpoise,
        habitat_bottlenose_dolphin = habitat_bottlenose_dolphin,
        habitat_common_dolphin = habitat_common_dolphin,
        habitat_harbour_seal = habitat_harbour_seal,
        habitat_all = unlist(
          list(
            habitat_harbour_porpoise,
            habitat_bottlenose_dolphin,
            habitat_common_dolphin,
            habitat_harbour_seal
          ),
          use.names = FALSE
        )
      )
    })
  })
}
