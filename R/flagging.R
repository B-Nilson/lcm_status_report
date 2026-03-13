apply_purpleair_flags <- function(flagged_obs, value_cols_flagged) {
  flagged_obs |>
    dplyr::mutate(
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
      # Combine A/B mean assuming nothing is flagged
      pm25 = elementwise_mean_no_na(pm25_a, pm25_b),
      # Combine A/B mean after censoring flagged data
      pm25_flagged = elementwise_mean_no_na(pm25_a_flagged, pm25_b_flagged),
      pm25_flagged = ifelse(pm25_flag, NA, pm25_flagged)
    )
}

add_purpleair_flags <- function(obs) {
  obs |>
    dplyr::group_by(site_id) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # Flag failed pm25 sensors (use flag from db)
      pm25_a_flag = pm25_qc %in% c(1, 3),
      pm25_b_flag = pm25_qc %in% 2:3,
      pm25_flag = pm25_qc == 4 | is.na(pm25_qc)
    ) |>
    flag_bad_temperature() |>
    flag_bad_humidity() |>
    dplyr::ungroup()
}

flag_bad_temperature <- function(obs) {
  allowed_steps <- list(
    '1 hours' = 10,
    '2 hours' = 15,
    '3 hours' = 20,
    '6 hours' = 30,
    '12 hours' = 40
  )
  obs |>
    quacker::qaqc_timeseries(
      date_col = "date",
      value_cols = "temperature",
      time_step = "10 mins",
      allowed_range = c(-50, 50),
      allowed_steps = allowed_steps,
      allowed_repeats = 3 * 6 # 3 hours of 10 min obs
    ) |>
    dplyr::select(-.flags_temperature, -.flag_name_temperature) |>
    dplyr::rename(temperature_flag = .flag_temperature)
}

flag_bad_humidity <- function(obs) {
  obs |>
    quacker::qaqc_timeseries(
      date_col = "date",
      value_cols = "rh",
      time_step = "10 mins",
      allowed_range = c(0, 100),
      allowed_steps = list('1 hours' = 40),
      allowed_repeats = 3
    ) |>
    dplyr::select(-.flags_rh, -.flag_name_rh) |>
    dplyr::rename(rh_flag = .flag_rh)
}
