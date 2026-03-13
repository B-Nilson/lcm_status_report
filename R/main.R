source("R/parse_params.R")
source("R/make_aqsu_status_map.R")
params <- parse_params("params.yml")

# pass params to make_aqsu_status_map
make_aqsu_status_map(
  latest_date = params$report_period$latest_date,
  duration_days = params$report_period$duration_days,
  averaging_period = params$report_period$averaging_period,
  p_hours_flagged_thresh = params$display$p_hours_flagged_thresh,
  meta_cols = params$columns$meta |> unlist(),
  value_cols = params$columns$values |> unlist(),
  obs_cache_rds = params$.obs_cache_rds,
  timestamp_tz = params$display$timestamp_tz,
  base_map_provider = params$display$base_map_provider,
  report_dir = params$dirs$output,
  img_dir = params$dirs$plots,
  map_lib_dir = params$dirs$map_libs,
  css_dir = params$dirs$css,
  js_dir = params$dirs$js,
  map_path = params$dirs$output |> file.path(params$files$map),
  data_path = params$dirs$output |> file.path(params$files$data),
  page_title = params$display$page_title,
  popup_width_px = params$display$popup_width_px,
  save_figures = params$files$save_figures
)
