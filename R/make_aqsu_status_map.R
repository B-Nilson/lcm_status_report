# TODO: priority list for repair/replace with info on install/failure date, what failed, env. conditions ecposed to
make_aqsu_status_map <- function(
  latest_date = lubridate::now("UTC"),
  duration_days = 7,
  averaging_period = "10 mins",
  p_hours_flagged_thresh = 0.15,
  meta_cols,
  value_cols,
  obs_cache_rds = "data/aqsu_past_%s_days.rds" |> sprintf(duration_days),
  timestamp_tz = "browser",
  base_map_provider = c("Light Theme" = "OpenStreetMap"),
  report_dir = "deployments",
  img_dir = "plots",
  map_lib_dir = "libs",
  css_dir = "css",
  js_dir = "js",
  map_path = "%s/index.html" |> sprintf(report_dir),
  data_path = "%s/aqsu_monitor_status.csv" |> sprintf(report_dir),
  page_title = "AQSU Status",
  popup_width_px = 600,
  save_figures = TRUE # set to FALSE for faster testing
) {
  # Import fonts and functions
  # extrafont::font_import() # only run once per machine
  # extrafont::loadfonts(device = "all", quiet = TRUE)

  source("R/load_report_data.R")
  source("R/format_data.R")
  source("R/flagging.R")
  source("R/map_controls.R")
  source("R/monitor_markers.R")
  source("R/marker_hovers.R")
  source("R/marker_popups.R")
  source("R/plotting.R")

  js_css_paths <- file.path(css_dir, "report_stylesheet.css") |>
    c(file.path(js_dir, c("main.js", "helpers.js")))
  inter_font_url <- '%s?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap' |>
    sprintf("https://fonts.googleapis.com/css2") |>
    setNames("Inter")
  earliest_date <- latest_date - lubridate::days(duration_days)
  desired_cols <- c(meta_cols, value_cols)
  value_cols <- ifelse(names(value_cols) == "", value_cols, names(value_cols))

  value_cols_flagged <- value_cols |>
    stats::setNames(paste0(value_cols, "_flagged"))

  # Load Past 2 Weeks of AQSU Data and flag
  dirname(obs_cache_rds) |> dir.create(showWarnings = FALSE, recursive = TRUE)
  obs <- latest_date |>
    load_report_data(
      duration_days = duration_days,
      desired_cols = desired_cols,
      cache_path = obs_cache_rds
    ) |>
    # TODO: aggregate to averaging period if needed
    # Fill in from start of data record for a specific site to last date of data from any site
    complete_active_site_records(
      time_step = averaging_period,
      duration_days = duration_days
    ) |>
    # Flag temperature/rh data and breakdown pm25 flag
    add_purpleair_flags() |>
    apply_purpleair_flags(value_cols_flagged = value_cols_flagged) |>
    dplyr::arrange(site_id, date)

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
                save_figures = save_figures
              )
          }
        )
    )

  # Make Map of AQSU Flags
  layer_groups <- c(
    "PM2.5 Sensors",
    "Temperature Sensor",
    "Humidity Sensor",
    "Current PM2.5",
    "Current Temperature",
    "Current RH"
  )
  if (is.null(names(base_map_provider))) {
    names(base_map_provider) <- base_map_provider # TODO: do in aqmapr
  }
  map <- aqmapr::make_leaflet_map(
    base_maps = base_map_provider,
    layer_control_titles = NULL,
    add_basemaps_to_layer_control = FALSE,
    page_title = page_title,
    track_map_state = FALSE,
    center_on_opened_popup = TRUE,
    include_scalebar = FALSE
  ) |>
    leaflet.extras::addHash() |> # track map center/zoom
    aqmapr::include_font(font_urls = inter_font_url, force = TRUE) |>
    aqmapr::add_map_timestamp(
      timestamp = max(obs$date) |>
        lubridate::with_tz(ifelse(
          timestamp_tz == "browser",
          yes = "UTC",
          no = timestamp_tz
        )),
      use_browser_timezone = timestamp_tz == "browser"
    ) |>
    # Add layers control to topright
    leaflet::addLayersControl(
      overlayGroups = layer_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    add_monitor_markers(
      map_data = map_data,
      duration_days = duration_days,
      popup_width_px = popup_width_px
    ) |>
    # Add search menu and scale bar
    add_search_menu(
      target_groups = layer_groups[1],
      search_property = "label"
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    # Include custom JS/CSS
    aqmapr::include_scripts(paths = js_css_paths) |>
    htmlwidgets::onRender("handle_render")

  # Save map to html if desired
  if (!is.null(map_path)) {
    map |>
      aqmapr::save_map(
        save_to = map_path,
        library_dir = map_lib_dir
      )
  }

  # Save data if desired
  if (!is.null(data_path)) {
    map_data |>
      dplyr::select(-plot_data, -popup, -dplyr::starts_with("hover_")) |>
      data.table::fwrite(data_path)
  }

  invisible(map)
}
