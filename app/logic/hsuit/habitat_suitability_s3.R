##################################################################################
##################################################################################

# Author: Johannes Nowe
# Date: 2026-05-19
# Script Name: ~/DUC2_app/app/logic/hsuit/habitat_suitability_s3.R
# Script Description: load habitat suitability data layers from s3 bucket

##################################################################################
##################################################################################

box::use(
  terra[rast, minmax],
  leaflet[colorNumeric],
  httr[GET, write_disk, stop_for_status, timeout]
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
      layer_count = 12,
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
      layer_count = 2,
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
        layer_count = 9,
        scenario = scenario_id,
        scenario_label = scenario_specs[[scenario_id]],
        available = TRUE
      )
    }
  }

  cache_dir <- file.path(tempdir(), "duc2_habitat_suitability")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  layer_cache <- new.env(parent = emptyenv())
  palette_cache <- new.env(parent = emptyenv())
  domain_cache <- new.env(parent = emptyenv())

  load_layer <- function(info) {
    layer_file <- file.path(cache_dir, basename(info$file_name))

    tryCatch(
      {
        if (!file.exists(layer_file)) {
          response <- GET(
            info$url,
            write_disk(layer_file, overwrite = TRUE),
            timeout(120)
          )
          stop_for_status(response)
        }

        rast(layer_file)
      },
      error = function(error) {
        warning(
          paste0(
            "Could not load habitat suitability layer '",
            info$label,
            "' from ",
            info$url,
            ": ",
            conditionMessage(error)
          ),
          call. = FALSE
        )
        NULL
      }
    )
  }

  habitat_layers_info <- lapply(names(habitat_layers_info), function(id) {
    x <- habitat_layers_info[[id]]
    x$loaded <- FALSE
    x
  }) |>
    stats::setNames(names(habitat_layers_info))

  layer_domain <- function(r) {
    domain <- tryCatch(
      as.numeric(range(minmax(r), na.rm = TRUE)),
      error = function(error) c(0, 1)
    )

    if (
      length(domain) != 2 ||
        any(!is.finite(domain)) ||
        domain[1] == domain[2]
    ) {
      return(c(0, 1))
    }

    domain
  }

  get_layer <- function(layer_id) {
    info <- habitat_layers_info[[layer_id]]
    if (is.null(info) || !isTRUE(info$available)) {
      return(NULL)
    }

    if (exists(layer_id, envir = layer_cache, inherits = FALSE)) {
      r <- get(layer_id, envir = layer_cache)
    } else {
      r <- load_layer(info)
      if (is.null(r)) {
        return(NULL)
      }

      assign(layer_id, r, envir = layer_cache)
      info$loaded <- TRUE
      habitat_layers_info[[layer_id]] <<- info
    }

    if (exists(layer_id, envir = domain_cache, inherits = FALSE)) {
      domain <- get(layer_id, envir = domain_cache)
    } else {
      domain <- layer_domain(r)
      assign(layer_id, domain, envir = domain_cache)
    }

    if (exists(layer_id, envir = palette_cache, inherits = FALSE)) {
      palette <- get(layer_id, envir = palette_cache)
    } else {
      palette <- colorNumeric("viridis", domain, na.color = "transparent")
      assign(layer_id, palette, envir = palette_cache)
    }

    list(
      raster = r,
      palette = palette,
      domain = domain
    )
  }

  list(
    habitat_layers_info = habitat_layers_info,
    habitat_layers = layer_cache,
    habitat_palettes = palette_cache,
    habitat_domains = domain_cache,
    get_layer = get_layer
  )
}
