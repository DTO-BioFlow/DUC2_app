##################################################################################
##################################################################################

# Author: Johannes Nowe
# Date: 2026-05-19
# Script Name: ~/DUC2_app/app/logic/hsuit/habitat_suitability_s3.R
# Script Description: load habitat suitability data layers from s3 bucket

##################################################################################
##################################################################################

box::use(
  terra[rast, values],
  leaflet[colorNumeric],
  httr[GET, write_disk, stop_for_status]
)

load_habitat_suitability_s3 <- function(s3_bucket_url) {
  layer_url <- function(file_name) {
    paste0(sub("/?$", "/", s3_bucket_url), file_name)
  }

  species_specs <- list(
    harbour_porpoise = list(label = "Harbour porpoise", file = "porpoise"),
    bottlenose_dolphin = list(label = "Bottlenose dolphin", file = "bottlenose"),
    common_dolphin = list(label = "Common dolphin", file = "dolphin"),
    harbour_seal = list(label = "Harbour seal", file = "seal")
  )
  scenario_specs <- list(
    ssp119 = "SSP1-1.9",
    ssp126 = "SSP1-2.6",
    ssp245 = "SSP2-4.5",
    ssp370 = "SSP3-7.0",
    ssp460 = "SSP4-6.0",
    ssp585 = "SSP5-8.5"
  )

  habitat_layers_info <- list()

  for (species_id in names(species_specs)) {
    species <- species_specs[[species_id]]

    monthly_id <- paste(species_id, "present_monthly", sep = "_")
    monthly_file <- paste0("monthly_", species$file, ".nc")
    habitat_layers_info[[monthly_id]] <- list(
      species = species_id,
      period = "present_monthly",
      label = paste(species$label, "present monthly", sep = " - "),
      file_name = monthly_file,
      url = layer_url(monthly_file),
      monthly = TRUE,
      scenario = "present",
      scenario_label = "Present",
      available = TRUE
    )

    present_id <- paste(species_id, "present_decade", sep = "_")
    present_file <- paste0("decadal_", species$file, "_present.nc")
    habitat_layers_info[[present_id]] <- list(
      species = species_id,
      period = "present_decade",
      label = paste(species$label, "present decade", sep = " - "),
      file_name = present_file,
      url = layer_url(present_file),
      monthly = FALSE,
      scenario = "present",
      scenario_label = "Present",
      available = TRUE
    )

    for (scenario_id in names(scenario_specs)) {
      scenario_number <- sub("^ssp", "", scenario_id)
      future_id <- paste(species_id, "future_decade", scenario_id, sep = "_")
      future_file <- paste0(
        "decadal_",
        species$file,
        "_future_ssp",
        scenario_number,
        ".nc"
      )

      habitat_layers_info[[future_id]] <- list(
        species = species_id,
        period = "future_decade",
        label = paste(
          species$label,
          "future decade",
          scenario_specs[[scenario_id]],
          sep = " - "
        ),
        file_name = future_file,
        url = layer_url(future_file),
        monthly = FALSE,
        scenario = scenario_id,
        scenario_label = scenario_specs[[scenario_id]],
        available = TRUE
      )
    }
  }

  load_layer <- function(url, label) {
    tf <- tempfile(fileext = ".nc")
    tryCatch(
      {
        response <- GET(url, write_disk(tf, overwrite = TRUE))
        stop_for_status(response)
        rast(tf)
      },
      error = function(error) {
        warning(
          paste0(
            "Could not load habitat suitability layer '",
            label,
            "' from ",
            url,
            ": ",
            conditionMessage(error)
          ),
          call. = FALSE
        )
        NULL
      }
    )
  }

  available_layers_info <- Filter(function(x) {
    isTRUE(x$available)
  }, habitat_layers_info)

  habitat_layers <- lapply(available_layers_info, function(x) {
    load_layer(x$url, x$label)
  })
  habitat_layers <- Filter(Negate(is.null), habitat_layers)

  loaded_layer_ids <- names(habitat_layers)
  habitat_layers_info <- lapply(names(habitat_layers_info), function(id) {
    x <- habitat_layers_info[[id]]
    x$loaded <- id %in% loaded_layer_ids
    x
  }) |>
    stats::setNames(names(habitat_layers_info))

  habitat_palettes <- lapply(habitat_layers, function(r) {
    colorNumeric("viridis", values(r), na.color = "transparent")
  })

  list(
    habitat_layers_info = habitat_layers_info,
    habitat_layers = habitat_layers,
    habitat_palettes = habitat_palettes
  )
}
