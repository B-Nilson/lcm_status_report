make_marker_hovers <- function(
  map_data,
  sensors = c("pm25", "temperature", "rh"),
  is_flagged_prefix = "is_flagged_",
  hover_prefix = "hover_",
  is_offline_suffix = "_currently_offline",
  duration_days,
  flag_threshold
) {
  hover_columns <- paste0(is_flagged_prefix, sensors) |>
    setNames(paste0(hover_prefix, sensors))
  map_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(hover_columns),
        \(is_flagged) {
          offline_col <- dplyr::cur_column() |>
            sub(pattern = is_flagged_prefix, replacement = "") |>
            paste0(is_offline_suffix)
          map_data |>
            handyr::sf_as_df() |>
            aqsu_status_hover(
              is_bad = is_flagged,
              is_offline = map_data[[offline_col]],
              duration_days = duration_days,
              flag_threshold = flag_threshold
            )
        }
      )
    )
}

aqsu_status_hover <- function(
  dat,
  is_bad = FALSE,
  is_offline = TRUE,
  duration_days,
  flag_threshold = 0.15
) {
  dat <- dat |>
    dplyr::mutate(
      is_bad = is_bad & !.data$entirely_offline,
      is_offline = is_offline | .data$entirely_offline | .data$currently_offline
    )
  templates <- list(
    missing = c(
      "<big><strong>%s</strong></big>",
      "ID: %s",
      "<b>No data available past %s days</b>",
      "Last Seen: %s"
    ),
    default = c(
      "<big><strong>%s</strong></big>",
      "ID: %s",
      "# of obs past %s days: %s",
      "%s",
      "<b>Click to view timeseries w/ flags</b><hr>Status: <b>%s</b>",
      "Raised flags for at least %s%% of hours: <b>%s</b>",
      "%s"
    )
  ) |>
    lapply(paste, collapse = "<br>")

  status_message <- dplyr::case_when(
    dat$currently_offline ~ "Entire monitor currently offline",
    dat$is_offline ~ "Sensor currently offline",
    dat$is_bad ~ "Suspect bad data",
    .default = "Appears to be good"
  )
  raised_flags <- ifelse(dat$flags == "", "None", dat$flags)
  summaries <- dat |> make_flagged_messages()

  dplyr::case_when(
    dat$entirely_offline ~ templates$missing |>
      sprintf(
        dat$name,
        dat$site_id,
        duration_days,
        dat$last_seen |> format("%B %d (%Y) at %H:%M (UTC)")
      ),
    .default = templates$default |>
      sprintf(
        dat$name,
        dat$site_id,
        duration_days,
        dat$n_obs,
        dat$date_range,
        status_message,
        flag_threshold * 100,
        raised_flags,
        summaries
      )
  )
}

make_flagged_messages <- function(dat, stats = c(mean = "mean", max = "max")) {
  sensors <- list(
    A = list(key = "pm25_a", pretty = "PM<sub>2.5</sub> A"),
    B = list(key = "pm25_b", pretty = "PM<sub>2.5</sub> B"),
    AB = list(key = "pm25", pretty = "Combined A/B"),
    T = list(key = "temperature", pretty = "Temp."),
    RH = list(key = "rh", pretty = "Humidity")
  )

  # Extract relevant columns for each list entry
  summaries <- names(sensors) |>
    setNames(names(sensors)) |>
    lapply(\(flag_name) {
      c(
        list(
          values = lapply(stats, \(stat) {
            columns = "%s_%s%s" |>
              sprintf(sensors[[flag_name]]$key, c("", "flagged_"), stat) |>
              setNames(c("before", "after"))
            dat |> dplyr::select(dplyr::any_of(columns))
          })
        ),
        list(
          p_flagged = "%s_p_flagged" |> sprintf(sensors[[flag_name]]$key),
          median_flag = "%s_flag_name_median" |>
            sprintf(sensors[[flag_name]]$key)
        ) |>
          lapply(\(col) {
            vals <- dat |> dplyr::select(dplyr::any_of(col))
            if (ncol(vals) == 0) NULL else vals[[col]]
          })
      )
    })

  # Make summary messages united with "<br>" (1 value for each row of dat)
  messages <- names(sensors) |>
    setNames(names(sensors)) |>
    lapply(\(flag_name) {
      flag_name |>
        flagged_message(
          sensor = sensors[[flag_name]]$pretty,
          values = summaries[[flag_name]]$values,
          p_flagged = summaries[[flag_name]]$p_flagged,
          median_flag = summaries[[flag_name]]$median_flag
        )
    }) |>
    unite_w_newline()

  ifelse(nchar(messages) == 0, "", "All raised flags:<br>") |>
    paste0(messages)
}

flagged_message <- function(
  flag_name = c("A", "B", "AB", "T", "RH")[1],
  sensor = "PM<sub>2.5</sub> A",
  values,
  p_flagged,
  median_flag = NULL
) {
  template <- "&emsp; %s: %s -> %s"
  value_summary <- names(values) |>
    setNames(names(values)) |>
    lapply(\(stat) {
      stat_pretty <- stringr::str_to_title(stat)
      vals <- values[[stat]] |>
        lapply(\(x) {
          x |>
            round(digits = 1) |>
            as.character() |>
            dplyr::replace_values(c(NA_character_, "NaN") ~ "No data.")
        })
      template |> sprintf(stat_pretty, vals$before, vals$after)
    }) |>
    unite_w_newline()

  if (!is.null(median_flag)) {
    median_flag_msg <- "&emsp;Values are: %s<br>" |> sprintf(median_flag)
    median_flag_msg <- ifelse(is.na(median_flag), "", median_flag_msg)
  } else {
    median_flag_msg <- ""
  }
  message <- paste0(
    "-[%s] %s sensor: %s of obs flagged<br>",
    median_flag_msg,
    "%s"
  ) |>
    sprintf(
      flag_name,
      sensor,
      round(p_flagged * 100, 1) |> paste0("%"),
      value_summary
    )

  ifelse(p_flagged > 0, message, NA)
}
