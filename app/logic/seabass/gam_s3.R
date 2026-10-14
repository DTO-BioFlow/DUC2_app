##################################################################################
##################################################################################
# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-01-29
# Script Name: ~/DUC2_viewer_acoustic_telemetry/R/helpers_load_acoustic_telemetry_GAM_s3.R
# Script Description: load data layers from s3 bucket resulting from GAM modeling workflow
##################################################################################
##################################################################################
# -----------------------
# Create example rasters
# (replace with rast("seabass.nc"), rast("porpoise.nc"))
# -----------------------
box::use(
  httr[GET, write_disk],
  leaflet[colorNumeric],
  stats[runif],
  terra[ncell, rast, setValues, values],
)

load_acoustic_telemetry_GAM_s3 <- function(s3_bucket_url) {
  # TODO: check if make_raster() should be continued to determine the color scale for the
  # seabass layers
  make_raster <- function(seed) {
    set.seed(seed)
    r <- rast(
      nrows = 100,
      ncols = 100,
      xmin = 0,
      xmax = 5,
      ymin = 50,
      ymax = 52,
      nlyrs = 12
    )
    r <- setValues(r, runif(ncell(r) * 12))
    names(r) <- month.name
    r
  }
  #
  r_seabass <- make_raster(1)

  # -----------------------
  # Palette
  # -----------------------
  pal_seabass <- colorNumeric(
    "viridis",
    domain = values(r_seabass),
    na.color = "transparent"
  )

  # -----------------------
  # List of layers
  # -----------------------
  prediction_layers_info <- list(
    "Predictions inside OWF" = list(
      url = paste0(s3_bucket_url, "predictions_inside_owf_median_months.nc"),
      monthly = TRUE
    ),
    "Predictions outside OWF" = list(
      url = paste0(s3_bucket_url, "predictions_outside_owf_median_months.nc"),
      monthly = TRUE
    ),
    "Diff OWF" = list(
      url = paste0(s3_bucket_url, "diff_owf.nc"),
      monthly = TRUE
    )
  )

  env_layers_info <- list(
    "Bathymetry" = list(
      url = paste0(s3_bucket_url, "bathy.nc"),
      monthly = FALSE
    ),
    "Habitats" = list(
      url = paste0(s3_bucket_url, "habitats.nc"),
      monthly = FALSE
    ),
    "LOD median months" = list(
      url = paste0(s3_bucket_url, "lod_median_months.nc"),
      monthly = TRUE
    ),
    "OWF distance" = list(
      url = paste0(s3_bucket_url, "owf_dist.nc"),
      monthly = FALSE
    ),
    "Shipwreck distance" = list(
      url = paste0(s3_bucket_url, "shipwreck_dist.nc"),
      monthly = FALSE
    ),
    "SST median months" = list(
      url = paste0(s3_bucket_url, "sst_median_months.nc"),
      monthly = TRUE
    ),
    "X coords" = list(
      url = paste0(s3_bucket_url, "x_m_4326.nc"),
      monthly = FALSE
    ),
    "Y coords" = list(
      url = paste0(s3_bucket_url, "y_m_4326.nc"),
      monthly = FALSE
    )
  )

  data_layers_info <- list(
    "Counts median months" = list(
      url = paste0(s3_bucket_url, "counts_median_months.nc"),
      monthly = TRUE
    ),
    "N active tags median months" = list(
      url = paste0(s3_bucket_url, "n_active_tags_median_months.nc"),
      monthly = TRUE
    )
  )

  # -----------------------
  # Load layers at startup
  # -----------------------
  load_layer <- function(url) {
    tf <- tempfile(fileext = ".nc")
    GET(url, write_disk(tf, overwrite = TRUE))
    rast(tf)
  }

  prediction_layers <- lapply(prediction_layers_info, function(x) {
    load_layer(x$url)
  })
  env_layers <- lapply(env_layers_info, function(x) load_layer(x$url))
  data_layers <- lapply(data_layers_info, function(x) load_layer(x$url))

  # -----------------------
  # Create palettes for layers
  # -----------------------
  prediction_palettes <- lapply(prediction_layers, function(r) {
    colorNumeric("viridis", values(r), na.color = "transparent")
  })

  env_palettes <- lapply(env_layers, function(r) {
    colorNumeric("viridis", values(r), na.color = "transparent")
  })

  data_palettes <- lapply(data_layers, function(r) {
    colorNumeric("viridis", values(r), na.color = "transparent")
  })

  # -----------------------
  # Save outputs needed in the app in a named list
  # -----------------------

  telemetry_gam_s3 <-
    list(
      prediction_layers = prediction_layers,
      env_layers = env_layers,
      data_layers = data_layers,
      pal_seabass = pal_seabass,
      prediction_palettes = prediction_palettes,
      env_palettes = env_palettes,
      data_palettes = data_palettes
    )

  telemetry_gam_s3
}
