make_marker_popups <- function(
  map_data,
  obs,
  value_cols,
  date_range,
  report_dir,
  img_dir,
  save_figures = TRUE
) {
  # Make timeseries data for each monitor
  plot_cols <- c(
    "site_id",
    "name",
    "date",
    value_cols,
    paste0(value_cols, "_flag"),
    paste0(value_cols, "_flag_name")
  )
  map_data$plot_data <- obs |>
    dplyr::select(dplyr::any_of(plot_cols)) |>
    dplyr::mutate(site_id_copy = site_id) |>
    tidyr::nest(.by = site_id_copy) |>
    dplyr::pull(data)

  # Make popup html, creating and saving figures along the way
  file.path(report_dir, img_dir) |>
    dir.create(showWarnings = FALSE, recursive = TRUE)
  map_data |>
    dplyr::mutate(
      popup = .data$plot_data |>
        handyr::for_each(
          .enumerate = TRUE,
          \(pd, i) {
            pd |>
              aqsu_status_popup(
                date_range = date_range,
                report_path = report_dir,
                img_dir = img_dir,
                width = popup_width_px,
                is_missing = map_data$entirely_offline[i],
                save_figures = save_figures
              )
          }
        )
    )
}

aqsu_status_popup <- function(
  plot_data,
  date_range = NULL,
  is_missing = FALSE,
  report_path = "./",
  img_dir = "plots",
  res = 300,
  width,
  units = "px",
  save_figures = TRUE
) {
  if (is.null(date_range)) {
    date_range <- range(plot_data$date)
  }
  # Save plots
  img_names <- paste0(
    "purpleair_",
    plot_data$site_id[1],
    "_qaqc_timeseries",
    c("", "_cleaned"),
    ".png"
  )
  img_paths <- file.path(report_path, img_dir, img_names)
  if (save_figures) {
    plot_data |>
      list(
        plot_data |>
          dplyr::mutate(
            pm25_a = ifelse(pm25_flag | pm25_a_flag, NA, pm25_a),
            pm25_b = ifelse(pm25_flag | pm25_b_flag, NA, pm25_b),
            temperature = ifelse(temperature_flag > 0, NA, temperature),
            rh = ifelse(rh_flag > 0, NA, rh),
            dplyr::across(dplyr::ends_with("_flag_name"), \(x) NA) # remove flag names as well
          )
      ) |>
      lapply(make_site_flag_plot, date_range = date_range) |>
      handyr::for_each(
        .enumerate = TRUE,
        .show_progress = FALSE,
        \(plot, i) {
          plot |>
            handyr::save_figure(img_paths[i], page_width = 7, taller = 1) |>
            suppressWarnings() |>
            handyr::on_error(.return = NULL)
        }
      )
  }

  # Return html img reference to plots
  img_paths_rel <- file.path(img_dir, img_names[1]) # js will switch for other image
  alt_text <- "Timeseries for past week with QA/QC flags from this monitor's PM2.5 and temperature sensors"

  dplyr::case_when(
    is_missing ~ "<center><b>No data available.</b></center>",
    TRUE ~
      paste0(
        '<img loading="lazy" src="',
        img_paths_rel,
        '" alt="',
        alt_text,
        '" width=',
        paste0(width, units),
        ' />
         <button onclick="change_plot(this)">Toggle Display of Flagged Values</button>'
      )
  )
}
