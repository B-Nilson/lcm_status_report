
aqsu_status_hover <- function(
  dat,
  is_bad = FALSE,
  is_missing = FALSE,
  duration_days = 14,
  flag_threshold = 0.15
) {
  if (length(is_missing) == 1) {
    is_missing <- rep(is_missing, nrow(dat))
  }
  if (length(is_bad) == 1) {
    is_bad <- rep(is_bad, nrow(dat))
  }
  with(
    dat,
    dplyr::case_when(
      is_missing ~
        paste0(
          "<big><strong>",
          name,
          "</strong></big><br>",
          "ID: ",
          site_id,
          "<br>",
          "<b>No data available past ",
          duration_days,
          " days</b><br>",
          "Last Seen: ",
          format(last_seen, "%B %d (%Y) at %H:%M (UTC)")
        ),

      TRUE ~
        paste0(
          "<big><strong>",
          name,
          "</strong></big><br>",
          "ID: ",
          site_id,
          "<br>",
          "# of obs past ",
          duration_days,
          " days: ",
          n_obs,
          "<br>",
          date_range,
          "<br>",
          "<b>Click to view timeseries w/ flags</b><hr>",
          ifelse(
            is_bad,
            "Status: <b>Suspect bad data</b><br>",
            "Status: <b>Appears to be good</b><br>"
          ),
          paste0(
            "Raised flags for at least ",
            flag_threshold * 100,
            "% of hours: <b>",
            ifelse(flags == "", "None", flags),
            "</b><br>"
          ),
          "All raised flags:<br>",
          ifelse(
            pm25_a_p_flagged > 0,
            paste0(
              "-[A] A sensor: ",
              round(pm25_a_p_flagged * 100, 1),
              "% of obs flagged",
              "<br>&emsp;Mean: ",
              pm25_a_mean,
              " -> ",
              pm25_a_flagged_mean,
              "<br>&emsp;Max: ",
              pm25_a_max,
              " -> ",
              pm25_a_flagged_max,
              "<br>"
            ),
            ""
          ),
          ifelse(
            pm25_b_p_flagged > 0,
            paste0(
              "-[B] B sensor: ",
              round(pm25_b_p_flagged * 100, 1),
              "% of obs flagged",
              "<br>&emsp;Mean: ",
              pm25_b_mean,
              " -> ",
              pm25_b_flagged_mean,
              "<br>&emsp;Max: ",
              pm25_b_max,
              " -> ",
              pm25_b_flagged_max,
              "<br>"
            ),
            ""
          ),
          ifelse(
            pm25_p_flagged > 0,
            paste0(
              "-[D] A/B Agreement: ",
              round(pm25_p_flagged * 100, 1),
              "% of obs flagged",
              "<br>&emsp;Mean: ",
              pm25_mean,
              " -> ",
              pm25_flagged_mean,
              "<br>&emsp;Max: ",
              pm25_max,
              " -> ",
              pm25_flagged_max,
              "<br>"
            ),
            ""
          ),
          ifelse(
            temperature_p_flagged > 0,
            paste0(
              "-[T] Temp. sensor: ",
              round(temperature_p_flagged * 100, 1),
              "% of obs flagged",
              "<br>&emsp;Values are: ",
              temperature_flag_median,
              "<br>&emsp;Mean: ",
              temperature_mean,
              " -> ",
              temperature_flagged_mean,
              "<br>&emsp;Max: ",
              temperature_max,
              " -> ",
              temperature_flagged_max,
              "<br>"
            ),
            ""
          ),
          ifelse(
            rh_p_flagged > 0,
            paste0(
              "-[RH] Humidity sensor: ",
              round(rh_p_flagged * 100, 1),
              "% of obs flagged",
              "<br>&emsp;Values are: ",
              rh_flag_median,
              "<br>&emsp;Mean: ",
              rh_mean,
              " -> ",
              rh_flagged_mean,
              "<br>&emsp;Max: ",
              rh_max,
              " -> ",
              rh_flagged_max,
              "<br>"
            ),
            ""
          )
        )
    )
  )
}