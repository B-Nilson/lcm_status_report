# Packages/Functions ------------------------------------------------------

source("R/status-functions.R")
source("R/load_report_data.R")
source("R/flagging.R")
source("R/map_controls.R")
source("R/monitor_markers.R")

# Import fonts and functions
# extrafont::font_import() # only run once per machine
# extrafont::loadfonts(device = "all", quiet = TRUE)

# Setup -------------------------------------------------------------------

# What data to display
latest_date <- lubridate::now("UTC")
duration_days <- 7
averaging_period <- "10 mins"
desired_cols <- c(
  "site_id",
  "date",
  pm25_qc = "qaqc_flag_pm2.5",
  pm25 = "pm2.5_validated",
  pm25_a = "pm2.5_a",
  pm25_b = "pm2.5_b",
  "temperature",
  "rh"
)
value_cols <- desired_cols[-(1:3)]
value_cols <- ifelse(names(value_cols) == "", value_cols, names(value_cols))

# How to display data
popup_width_px <- 600
page_title <- "AQSU Status"
p_hours_flagged_thresh <- 0.15

# Where to save things
report_dir <- "deployments"
img_dir <- "plots" # relative to report_dir
obs_cache_rds <- "data/aqsu_past_2_week.rds"
output_paths <- list(
  map = "deployments/index.html",
  data = "deployments/aqsu_monitor_status.csv"
)

# TODO: include as default in aqmapr
inter_font_url <- c(
  "Inter" = 'https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap'
)

earliest_date <- latest_date - lubridate::days(duration_days)

# Load Past 2 Weeks of AQSU Data ------------------------------------------

dirname(obs_cache_rds) |> dir.create(showWarnings = FALSE, recursive = TRUE)
obs <- latest_date |>
  load_report_data(
    duration_days = duration_days,
    desired_cols = desired_cols,
    cache_path = obs_cache_rds
  )

# Flag Data ---------------------------------------------------------------

# Add "_flagged" names for passing to dplyr::across()
value_cols_flagged <- value_cols |>
  stats::setNames(paste0(value_cols, "_flagged"))

obs <- obs |>
  # TODO: aggregate to averaging period if needed
  # Fill in from start of data record for a specific site to last date of data from any site
  complete_active_site_records(
    time_step = averaging_period,
    duration_days = duration_days
  ) |>
  # Flag temperature/rh data and breakdown pm25 flag # TODO: apply to RH as well
  add_purpleair_flags() |>
  dplyr::mutate(
    # Combine A/B mean assuming nothing is flagged
    pm25 = elementwise_mean_no_na(pm25_a, pm25_b),
    # Add "_flagged" columns with flagged values replaced with NA
    dplyr::across(
      dplyr::all_of(value_cols_flagged),
      \(x) {
        val_col <- dplyr::cur_column()
        flag_vals <- get(val_col |> paste0("_flag"))
        x[flag_vals > 0] <- NA
        return(x)
      }
    ),
    # Combine A/B mean after censoring flagged data
    pm25_flagged = elementwise_mean_no_na(pm25_a_flagged, pm25_b_flagged),
    pm25_flagged = ifelse(pm25_flag, NA, pm25_flagged),
    # Add missing obs (before flagging) flags
    pm25_a_missing = is.na(pm25_a),
    pm25_b_missing = is.na(pm25_b),
    pm25_missing = pm25_a_missing & pm25_b_missing,
    temperature_missing = is.na(temperature),
    rh_missing = is.na(rh)
  ) |>
  dplyr::arrange(site_id, date)

# Make summary dataset ----------------------------------------------------

# Summarise flagging for each monitor
map_data <- obs |>
  make_status_map_summary(
    value_cols = value_cols_flagged,
    flag_threshold = p_hours_flagged_thresh
  )

# Make timeseries data for each monitor
plot_cols <- c(
  "site_id",
  "name",
  "date",
  value_cols,
  paste0(value_cols, "_flag")
)
map_data$plot_data <- obs |>
  dplyr::select(dplyr::all_of(plot_cols)) |>
  dplyr::mutate(site_id_copy = site_id) |>
  tidyr::nest(.by = site_id_copy) |>
  dplyr::pull(data)

# Make popup html, creating and saving figures along the way
file.path(report_dir, img_dir) |>
  dir.create(showWarnings = FALSE, recursive = TRUE)
map_data <- map_data |>
  dplyr::mutate(
    popup = .data$plot_data |>
      handyr::for_each(
        .enumerate = TRUE,
        \(pd, i) {
          pd |>
            aqsu_status_popup(
              earliest_date = earliest_date,
              report_path = report_dir,
              img_dir = img_dir,
              width = popup_width_px,
              is_missing = map_data$entirely_offline[i],
              save_figures = TRUE # set to FALSE for faster testing
            )
        }
      )
  )

# Map of AQSU Flags -------------------------------------------------------

aqmapr::make_leaflet_map(
  base_maps = c(`Light Theme` = "OpenStreetMap"),
  layer_control_titles = NULL,
  add_basemaps_to_layer_control = FALSE,
  page_title = page_title,
  track_map_state = TRUE,
  center_on_opened_popup = TRUE,
  include_scalebar = FALSE
) |>
  aqmapr::include_font(font_urls = inter_font_url, force = TRUE) |>
  aqmapr::add_map_timestamp(
    timestamp = max(obs$date) |> lubridate::with_tz("America/Edmonton"),
    use_browser_timezone = FALSE
  ) |>
  # Add layers control to topright
  leaflet::addLayersControl(
    overlayGroups = c(
      "PM2.5 Sensors",
      "Temperature Sensor",
      "Humidity Sensor",
      "Current PM2.5",
      "Current Temperature",
      "Current RH"
    ),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) |>
  add_monitor_markers(
    map_data = map_data,
    duration_days = duration_days,
    popup_width_px = popup_width_px
  ) |>
  # Add search menu and scale bar
  add_search_menu(target_groups = "PM2.5 Sensors", search_property = "label") |>
  leaflet::addScaleBar(position = "bottomleft") |>
  # Include custom JS/CSS
  aqmapr::include_scripts(
    paths = c("css/report_stylesheet.css", "js/status_map.js")
  ) |>
  htmlwidgets::onRender("handle_render") |>
  # Save map to html page
  aqmapr::save_map(
    save_to = output_paths$map,
    library_dir = "libs"
  )

map_data |>
  dplyr::select(-plot_data, -popup, -dplyr::starts_with("hover_")) |>
  data.table::fwrite(output_paths$data)

# TODO: priority list for repair/replace with info on install/failure date, what failed, env. conditions ecposed to
