load_report_data <- function(
  latest_date,
  duration_days,
  desired_cols,
  cache_path = "data/report_data_cache.rds"
) {
  db <- connect_to_db() |>
    handyr::on_error(
      .return = NULL,
      .warn = "Failed to connect to database - using cached data."
    )
  if (is.null(db)) {
    server_file <- paste0(
      "https://aqmap.ca/aqmap/outputs/",
      basename(cache_path)
    )
    server_file |> download.file(destfile = cache_path, mode = "wb")
    return(readRDS(cache_path))
  } else {
    on.exit(RPostgres::dbDisconnect(db))
  }

  # Extract AQSU obs from database
  obs <- db |>
    get_AQSU_obs(
      end_time = latest_date,
      duration_days = duration_days,
      desired_cols = desired_cols
    )

  # Cache copy
  obs |> saveRDS(file = cache_path)
  return(obs)
}

connect_to_db <- function(local = TRUE, ...) {
  host <- ifelse(local, "localhost", "https://aqmap.ca/")
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "aqmap",
    host = host,
    port = Sys.getenv('aqmap_db_port'),
    user = Sys.getenv('aqmap_db_user'),
    password = Sys.getenv('aqmap_db_pass'),
    ...
  )
}

# Define data extraction function (default is 2 weeks of data)
get_AQSU_obs <- function(db, end_time, duration_days = 14, desired_cols) {
  tbls <- list(
    obs = "pa_obs",
    meta = "pa_meta"
  )
  meta_cols <- c("site_id", "lat", "lng", "name")
  col_order <- meta_cols |>
    c(ifelse(names(desired_cols) == "", desired_cols, names(desired_cols))) |>
    unique()

  # Create date range from desired end time and duration
  date_range <- c(end_time - lubridate::days(duration_days), end_time)

  # Get AQSU meta/obs in date_range
  aqsu_sites <- db |>
    handyr::read_from_database(
      table_name = tbls$meta,
      query_fun = \(df) df |> dplyr::filter(is_aqsu)
    )
  obs <- db |>
    handyr::read_from_database(
      table_name = tbls$obs,
      query_fun = \(df) {
        df |>
          dplyr::select(dplyr::all_of(desired_cols)) |>
          dplyr::filter(
            site_id %in% !!aqsu_sites$site_id,
            date |> dplyr::between(!!date_range[1], !!date_range[2])
          )
      }
    )

  obs |>
    dplyr::full_join(aqsu_sites, by = "site_id") |>
    # Handle sites with no data having NA dates (use last date of obs in database)
    dplyr::mutate(
      date = dplyr::case_when(
        is.na(date) & !is.na(last_date) ~ last_date,
        is.na(date) ~ last_seen,
        .default = date
      )
    ) |>
    dplyr::select(dplyr::all_of(col_order))
}
