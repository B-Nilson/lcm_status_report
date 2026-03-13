parse_params <- function(params_file = "params.yml") {
  params <- yaml::read_yaml(params_file)
  if (params$report_period$latest_date == "now") {
    params$report_period$latest_date <- lubridate::now("UTC")
  } else {
    params$latest_date <- params$report_period$latest_date |>
      lubridate::ymd_hms(tz = "UTC")
  }
  params$.obs_cache_rds <- params$dirs$output |>
    file.path(params$files$cache) |>
    sub(
      pattern = "{days}",
      replacement = params$report_period$duration_days,
      fixed = TRUE
    )
  return(params)
}
