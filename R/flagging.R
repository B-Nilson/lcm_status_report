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
    convert_pm25_qc_to_flags() |>
    dplyr::arrange(date) |>
    tidyr::nest(.by = site_id) |>
    dplyr::mutate(
      data = data |>
        lapply(\(df) {
          df |>
            flag_bad_temperature() |>
            flag_bad_humidity()
        })
    ) |>
    tidyr::unnest(data)
}

convert_pm25_qc_to_flags <- function(obs) {
  obs |>
    dplyr::mutate(
      # Flag failed pm25 sensors (use flag from db)
      .flag_pm25_is_A_bad = pm25_qc %in% c(1, 3),
      .flag_pm25_is_B_bad = pm25_qc %in% 2:3,
      .flag_pm25_is_AB_disagree = pm25_qc == 4 | is.na(pm25_qc)
    ) |>
    quacker:::combine_flags(value_cols = "pm25") |>
    tidyr::unnest(.flags_pm25) |>
    dplyr::rename(
      pm25_flag = .flag_pm25_is_AB_disagree,
      pm25_flag_name = .flag_name_pm25,
      pm25_a_flag = .flag_pm25_is_A_bad,
      pm25_b_flag = .flag_pm25_is_B_bad
    ) |>
    dplyr::select(-.flag_pm25)
}

flag_bad_temperature <- function(obs) {
  obs |>
    quacker::qaqc_timeseries(
      date_col = "date",
      value_cols = "temperature",
      time_step = "10 mins",
      allowed_range = c(-50, 60),
      allowed_steps = list('20 mins' = 20),
      allowed_repeats = 6 * 6 # 6 hours of 10 min obs
    ) |>
    dplyr::select(-.flags_temperature) |>
    dplyr::rename(
      temperature_flag = .flag_temperature,
      temperature_flag_name = .flag_name_temperature
    ) |> 
    dplyr::mutate(
      temperature_flag_name = dplyr::case_when(
        startsWith(temperature_flag_name, "out of range") ~ "Out of range", # don't care about other flags if oor
        temperature_flag_name == "repeating at range" ~ "Repeating",
        .default = temperature_flag_name |> stringr::str_to_sentence()
      )
    )
}

flag_bad_humidity <- function(obs) {
  obs |>
    quacker::qaqc_timeseries(
      date_col = "date",
      value_cols = "rh",
      time_step = "10 mins",
      allowed_range = c(0, 100),
      allowed_steps = list('20 mins' = 60),
      allowed_repeats = 6 * 6 # 6 hours of 10 min obs
    ) |>
    dplyr::select(-.flags_rh) |>
    dplyr::rename(rh_flag = .flag_rh, rh_flag_name = .flag_name_rh) |> 
    dplyr::mutate(
      rh_flag_name = dplyr::case_when(
        startsWith(rh_flag_name, "out of range") ~ "Out of range", # don't care about other flags if oor
        rh_flag_name == "repeating at Range" ~ "Repeating",
        .default = rh_flag_name |> stringr::str_to_sentence()
      )
    )
}
