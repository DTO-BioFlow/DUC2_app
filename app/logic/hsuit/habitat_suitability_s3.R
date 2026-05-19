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

  habitat_layers_info <- list(
    harbour_porpoise_present_monthly = list(
      species = "harbour_porpoise",
      period = "present_monthly",
      label = "Harbour porpoise - present monthly",
      file_name = "monthly_porpoise.nc",
      url = layer_url("monthly_porpoise.nc"),
      monthly = TRUE,
      scenario = "present",
      available = TRUE
    ),
    harbour_porpoise_present_decade = list(
      species = "harbour_porpoise",
      period = "present_decade",
      label = "Harbour porpoise - present decade",
      file_name = "decadal_porpoise_present.nc",
      url = layer_url("decadal_porpoise_present.nc"),
      monthly = FALSE,
      scenario = "present",
      available = TRUE
    ),
    harbour_porpoise_future_decade = list(
      species = "harbour_porpoise",
      period = "future_decade",
      label = "Harbour porpoise - future decade",
      file_name = "decadal_porpoise_future.nc",
      url = layer_url("decadal_porpoise_future.nc"),
      monthly = FALSE,
      scenario = "future",
      available = FALSE
    ),
    bottlenose_dolphin_present_monthly = list(
      species = "bottlenose_dolphin",
      period = "present_monthly",
      label = "Bottlenose dolphin - present monthly",
      file_name = "monthly_bottlenose.nc",
      url = layer_url("monthly_bottlenose.nc"),
      monthly = TRUE,
      scenario = "present",
      available = TRUE
    ),
    bottlenose_dolphin_present_decade = list(
      species = "bottlenose_dolphin",
      period = "present_decade",
      label = "Bottlenose dolphin - present decade",
      file_name = "decadal_bottlenose_present.nc",
      url = layer_url("decadal_bottlenose_present.nc"),
      monthly = FALSE,
      scenario = "present",
      available = TRUE
    ),
    bottlenose_dolphin_future_decade = list(
      species = "bottlenose_dolphin",
      period = "future_decade",
      label = "Bottlenose dolphin - future decade",
      file_name = "decadal_bottlenose_future.nc",
      url = layer_url("decadal_bottlenose_future.nc"),
      monthly = FALSE,
      scenario = "future",
      available = FALSE
    ),
    common_dolphin_present_monthly = list(
      species = "common_dolphin",
      period = "present_monthly",
      label = "Common dolphin - present monthly",
      file_name = "monthly_dolphin.nc",
      url = layer_url("monthly_dolphin.nc"),
      monthly = TRUE,
      scenario = "present",
      available = TRUE
    ),
    common_dolphin_present_decade = list(
      species = "common_dolphin",
      period = "present_decade",
      label = "Common dolphin - present decade",
      file_name = "decadal_dolphin_present.nc",
      url = layer_url("decadal_dolphin_present.nc"),
      monthly = FALSE,
      scenario = "present",
      available = TRUE
    ),
    common_dolphin_future_decade = list(
      species = "common_dolphin",
      period = "future_decade",
      label = "Common dolphin - future decade",
      file_name = "decadal_dolphin_future.nc",
      url = layer_url("decadal_dolphin_future.nc"),
      monthly = FALSE,
      scenario = "future",
      available = FALSE
    ),
    harbour_seal_present_monthly = list(
      species = "harbour_seal",
      period = "present_monthly",
      label = "Harbour seal - present monthly",
      file_name = "monthly_seal.nc",
      url = layer_url("monthly_seal.nc"),
      monthly = TRUE,
      scenario = "present",
      available = TRUE
    ),
    harbour_seal_present_decade = list(
      species = "harbour_seal",
      period = "present_decade",
      label = "Harbour seal - present decade",
      file_name = "decadal_seal_present.nc",
      url = layer_url("decadal_seal_present.nc"),
      monthly = FALSE,
      scenario = "present",
      available = TRUE
    ),
    harbour_seal_future_decade = list(
      species = "harbour_seal",
      period = "future_decade",
      label = "Harbour seal - future decade",
      file_name = "decadal_seal_future.nc",
      url = layer_url("decadal_seal_future.nc"),
      monthly = FALSE,
      scenario = "future",
      available = FALSE
    )
  )

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
