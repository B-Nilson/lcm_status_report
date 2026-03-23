make_status_map_summary <- function(obs, value_cols, duration_days, flag_threshold = 0.15) {
  handyr::log_step("\t- Making point summaries for map")
  values <- c("pm25", unname(value_cols)) |>
    c("pm25_flagged", names(value_cols))
  obs |>
    # Add missing obs (before flagging) flags
    dplyr::mutate(
      pm25_a_missing = is.na(pm25_a),
      pm25_b_missing = is.na(pm25_b),
      pm25_missing = pm25_a_missing & pm25_b_missing,
      temperature_missing = is.na(temperature),
      rh_missing = is.na(rh)
    ) |>
    dplyr::group_by(site_id, lng, lat, name) |>
    dplyr::summarise(
      .groups = "drop",
      n_obs = dplyr::n(),
      last_seen = max(date),
      date_range = date_range_text(date),
      # Get current values
      dplyr::across(
        values |> stats::setNames(paste0("current_", values)),
        \(x) x[date == max(date)]
      ),
      # Means and maxes before and after flagging
      dplyr::across(
        dplyr::all_of(values),
        list(
          mean = \(x) {
            mean(x, na.rm = TRUE) |>
              handyr::swap(NaN, with = NA) |>
              round(digits = 1)
          },
          max = \(x) handyr::max(x, na.rm = TRUE)
        )
      ),
      # Mark if monitor is currently or entirly offline during the period
      currently_offline = pm25_missing[date == max(date)] &
        temperature_missing[date == max(date)],
      entirely_offline = all(pm25_missing & temperature_missing),
      p_offline = sum(pm25_missing & temperature_missing) / dplyr::n(),
      # Get current offline/online status for each variable and % offline or flagged
      dplyr::across(
        values[!endsWith(values, "_flagged")],
        list(
          currently_offline = \(x) {
            is_missing <- get(paste0(dplyr::cur_column(), "_missing"))
            is_missing[date == max(date)]
          },
          p_offline = \(x) {
            is_missing <- get(paste0(dplyr::cur_column(), "_missing"))
            sum(is_missing) / dplyr::n()
          },
          p_flagged = \(x) {
            is_flagged <- get(paste0(dplyr::cur_column(), "_flag")) > 0
            is_missing <- get(paste0(dplyr::cur_column(), "_missing"))
            sum(is_flagged & !is_missing) / dplyr::n()
          }
        )
      ),
      # Determine the most common temperature flag (in case multiple issues)
      dplyr::across(
        c("temperature_flag_name", "rh_flag_name"),
        list(median = \(x) most_common(x[x != "Missing"], na.rm = TRUE))
      ),
      # Group flags raised for easier mapping
      flag_group_pm = dplyr::case_when(
        entirely_offline ~ "No Data Available",
        currently_offline ~ "Monitor Currently Offline",
        pm25_a_currently_offline |
          pm25_b_currently_offline ~ "Sensor(s) Currently Offline",
        pm25_a_p_flagged > flag_threshold |
          pm25_b_p_flagged > flag_threshold ~ "A or B Sensor Flagged",
        pm25_p_flagged > flag_threshold ~ "A and B Sensors Disagree",
        TRUE ~ "No Detected Issue"
      ) |>
        factor(c(
          "No Data Available",
          "No Detected Issue",
          "Monitor Currently Offline",
          "Sensor(s) Currently Offline",
          "A or B Sensor Flagged",
          "A and B Sensors Disagree"
        )),
      flag_group_t = dplyr::case_when(
        entirely_offline ~ "No Data Available",
        currently_offline ~ "Monitor Currently Offline",
        temperature_currently_offline ~ "Sensor Currently Offline",
        temperature_p_flagged > flag_threshold ~ "Sensor Flagged",
        TRUE ~ "No Detected Issue"
      ) |>
        factor(c(
          "No Data Available",
          "No Detected Issue",
          "Monitor Currently Offline",
          "Sensor Currently Offline",
          "Sensor Flagged"
        )),
      flag_group_rh = dplyr::case_when(
        entirely_offline ~ "No Data Available",
        currently_offline ~ "Monitor Currently Offline",
        rh_currently_offline ~ "Sensor Currently Offline",
        rh_p_flagged > flag_threshold ~ "Sensor Flagged",
        TRUE ~ "No Detected Issue"
      ) |>
        factor(c(
          "No Data Available",
          "No Detected Issue",
          "Monitor Currently Offline",
          "Sensor Currently Offline",
          "Sensor Flagged"
        )),
      is_flagged_pm25 = flag_group_pm != "No Detected Issue",
      is_flagged_temperature = flag_group_t != "No Detected Issue",
      is_flagged_rh = flag_group_rh != "No Detected Issue",
      flags = paste(
        ifelse(pm25_a_p_flagged >= flag_threshold, "[A]", NA),
        ifelse(pm25_b_p_flagged >= flag_threshold, "[B]", NA),
        ifelse(pm25_p_flagged >= flag_threshold, "[AB]", NA),
        ifelse(temperature_p_flagged >= flag_threshold, "[T]", NA),
        ifelse(rh_p_flagged >= flag_threshold, "[RH]", NA),
        sep = ", "
      ) |>
        stringr::str_remove_all(", NA|NA, |NA")
    ) |>
    dplyr::arrange(site_id) |>
    # Convert to spatial
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
}

complete_active_site_records <- function(
  df,
  time_step = "1 hours",
  latest_date = lubridate::now("UTC"),
  duration_days = 14
) {
  earliest_date <- latest_date - lubridate::days(duration_days)
  completed <- df |>
    dplyr::group_by(
      site_id,
      lat,
      lng,
      name
    ) |>
    dplyr::filter(date >= earliest_date) |>
    tidyr::complete(
      date = seq(min(date), max(df$date, na.rm = TRUE), time_step)
    ) |>
    dplyr::ungroup()

  completed |>
    dplyr::bind_rows(df |> dplyr::filter(date < earliest_date))
}

date_range_text <- function(date) {
  paste(
    min(date) |> format("%F %H:%M (UTC)"),
    "to",
    max(date) |> format("%F %H:%M (UTC)")
  )
}