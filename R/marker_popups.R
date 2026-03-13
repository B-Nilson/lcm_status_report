aqsu_status_popup <- function(
  plot_data,
  earliest_date,
  is_missing = FALSE,
  report_path = "./",
  img_dir = "plots",
  res = 300,
  width,
  units = "px",
  save_figures = TRUE
) {
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
            rh = ifelse(rh_flag > 0, NA, rh)
          )
      ) |>
      lapply(make_site_flag_plot, earliest_date = earliest_date) |>
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
