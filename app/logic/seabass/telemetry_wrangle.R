box::use(
  arrow[read_parquet],
  dplyr[
    mutate,
    group_by,
    ungroup,
    n,
    summarise,
    first,
    rename,
    distinct,
    arrange,
    transmute,
    select,
    starts_with,
    left_join,
    across,
    all_of
  ],
  lubridate[floor_date],
  sf[st_drop_geometry],
  tidyr[pivot_wider, expand_grid, replace_na]
)

load_etn_dataset <- function(wms_layers, key = "seabass acoustic detections") {
  # TODO: replace with JHN's function to read in an etn dataset
  read_parquet(wms_layers[[key]]$wms_link)
}

summarise_etn_monthyear_individual <- function(etn_dataset) {
  etn_dataset |>
    mutate(
      monthyear = floor_date(datetime, unit = "months")
    ) |>

    # totals per month
    group_by(monthyear) |>
    mutate(n_detections_monthyear = n()) |>
    ungroup() |>

    # totals per month x station
    group_by(monthyear, station_name) |>
    mutate(n_detections_monthyear_station = n()) |>
    ungroup() |>

    # individual counts per month x station x tag
    group_by(monthyear, station_name, tag_serial_number) |>
    summarise(
      deploy_latitude = mean(latitude, na.rm = TRUE),
      deploy_longitude = mean(longitude, na.rm = TRUE),
      n_detections = n(),
      n_detections_monthyear = first(n_detections_monthyear),
      n_detections_monthyear_station = first(
        n_detections_monthyear_station
      ),
      .groups = "drop"
    )
}

build_monthyear_rds <- function(
  output_path,
  wms_layer_metadata,
  dataset_key
) {
  if (!file.exists(output_path)) {
    #if summary file does not yet exist -> make it running `summarise_etn_monthyear_individual()`
    # # TODO: change so that load_etn_dataset is used, but for now this breaks the submodule_acoustic_telemetry_data.R plots
    # etn_dataset <- load_etn_dataset(wms_layers, key = dataset_key)

    # if STAC etn data are unavailable - to remove later
    etn_dataset <-
      readRDS(file.path(dirname(output_path), "TEL_detections.rds")) |>
      rename(
        latitude = deploy_latitude,
        longitude = deploy_longitude,
        datetime = date_time
      )

    etn_sum <- summarise_etn_monthyear_individual(etn_dataset)
    # write summary df as .rds
    saveRDS(etn_sum, output_path)
  }
  etn_sum <- readRDS(output_path)

  # checks for dublicates
  stopifnot(
    !anyDuplicated(etn_sum[c("monthyear", "station_name", "tag_serial_number")])
  )

  # return .rds
  invisible(etn_sum)
}

# prepare minicharts leaflet inputs

minicharts_inputs_cache <- new.env(parent = emptyenv())

prep_minicharts_inputs <- function(deployments, etn_monthyear_individual_sum) {
  cache_key <- paste(
    nrow(deployments),
    nrow(etn_monthyear_individual_sum),
    sep = ":"
  )
  if (exists(cache_key, envir = minicharts_inputs_cache, inherits = FALSE)) {
    return(get(cache_key, envir = minicharts_inputs_cache))
  }

  if (inherits(deployments, "sf")) {
    deployments <- st_drop_geometry(deployments)
  }

  # stations: ensure exactly 1 row per station_name
  stations <- deployments |>
    group_by(station_name) |>
    summarise(
      lat = mean(deploy_latitude, na.rm = TRUE),
      lon = mean(deploy_longitude, na.rm = TRUE),
      .groups = "drop"
    ) |>
    distinct(station_name, lon, lat) |>
    arrange(station_name)

  # detections wide
  detections_wide <- etn_monthyear_individual_sum |>
    transmute(
      month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01")),
      station_name,
      tag_serial_number = paste0("id_", tag_serial_number),
      n_detections
    ) |>
    pivot_wider(
      id_cols = c(month, station_name),
      names_from = tag_serial_number,
      values_from = n_detections,
      values_fill = 0,
      values_fn = sum # safety: if duplicates exist, sum them
    )

  ids <- names(detections_wide |> select(starts_with("id_")))
  months <- sort(unique(detections_wide$month))

  anim_df <- expand_grid(
    month = months,
    station_name = stations$station_name
  ) |>
    left_join(detections_wide, by = c("month", "station_name")) |>
    mutate(across(
      all_of(ids),
      ~ replace_na(.x, 0)
    )) |>
    arrange(month, station_name) |>
    left_join(stations, by = "station_name")

  # size scaling
  station_month_totals <- etn_monthyear_individual_sum |>
    mutate(
      month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))
    ) |>
    group_by(month, station_name) |>
    summarise(
      n_station = sum(n_detections, na.rm = TRUE),
      .groups = "drop"
    )

  month_totals <- station_month_totals |>
    group_by(month) |>
    summarise(n_month = sum(n_station, na.rm = TRUE), .groups = "drop")

  anim_df <- anim_df |>
    left_join(station_month_totals, by = c("month", "station_name")) |>
    left_join(month_totals, by = "month") |>
    mutate(
      n_station = replace_na(n_station, 0),
      n_month = pmax(replace_na(n_month, 0), 1),
      rel = n_station / n_month,
      pie_size = 12 + 80 * sqrt(rel)
    )

  width_all <- anim_df$pie_size
  height_all <- anim_df$pie_size

  # integrity checks (catch your “row mismatch” early)
  stopifnot(
    nrow(anim_df) == length(anim_df$month),
    nrow(anim_df) == length(width_all),
    nrow(anim_df) == length(height_all),
    nrow(anim_df) == length(anim_df$station_name),
    all(stations$station_name %in% anim_df$station_name)
  )

  result <- list(
    stations = stations,
    anim_df = anim_df,
    ids = ids,
    months = months,
    width_all = width_all,
    height_all = height_all
  )

  assign(cache_key, result, envir = minicharts_inputs_cache)
  result
}
