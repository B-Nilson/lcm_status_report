addLegendCustom <- function(map, colors, labels, sizes, ...) {
  colorAdditions <- paste0(
    colors,
    "; vertical-align: middle; border-radius: 50%; width:",
    sizes,
    "px; height:",
    sizes,
    "px"
  )
  labelAdditions <- paste0(
    "<div style='display: inline-block;height: ",
    sizes,
    "px;margin-top: 4px;line-height: ",
    sizes,
    "px;'>",
    labels,
    "</div>"
  )

  return(leaflet::addLegend(
    map,
    colors = colorAdditions,
    labels = labelAdditions,
    ...
  ))
}

complete_active_site_records <- function(
  df,
  time_step = "1 hours",
  duration_days = 14
) {
  earliest_date <- lubridate::with_tz(Sys.time(), "UTC") -
    lubridate::days(duration_days)
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

mean_round_no_na <- function(x) {
  mean(x, na.rm = TRUE) |>
    round(1) |>
    handyr::swap(NA, with = "No Data.")
}

max_round_no_na <- function(x) {
  handyr::max(x, na.rm = TRUE) |>
    round(1) |>
    handyr::swap(NA, with = "No Data.")
}

date_range_text <- function(date) {
  paste(
    min(date) |> format("%F %H:%M (UTC)"),
    "to",
    max(date) |> format("%F %H:%M (UTC)")
  )
}

add_theme_300dpi <- function() {
  base_size <- 5
  ggplot2::theme(
    # Use Inter font for all text
    text = ggplot2::element_text(family = "Inter"),
    # Set font sizes based on base_size
    axis.text = ggplot2::element_text(size = base_size * 0.8),
    axis.title = ggplot2::element_text(size = base_size),
    legend.text = ggplot2::element_text(size = base_size * 0.8),
    legend.title = ggplot2::element_text(size = base_size * 0.8, face = "bold"),
    plot.title = ggplot2::element_text(
      size = base_size * 0.8,
      # Bold and center the title
      face = "bold",
      hjust = 0.5
    ),
    # Make axis lines/ticks less thick
    axis.line = ggplot2::element_line(linewidth = 0.1),
    axis.ticks = ggplot2::element_line(linewidth = 0.1),
    axis.ticks.length = ggplot2::unit(2, "pt"),
    # Adjust spacing around plot elements
    plot.margin = ggplot2::margin(2, 1, 1, 1), # Reduce spacing around plot
    legend.box.margin = ggplot2::margin(r = 3, 0, 0, 0), # stop long legend items from being cutoff
    legend.margin = ggplot2::margin(1, 1, 1, 1), # Reduce spacing between legends
    legend.spacing = ggplot2::unit(1, "pt"), # Reduce spacing between legends
    legend.box.spacing = ggplot2::unit(0, "pt"), # reduce spacing between plot and legend
    # legend.box.spacing = ggplot2::unit(0, "pt"),
    legend.key.size = ggplot2::unit(5, "pt"), #  reduce size of legends
    # Format legend
    legend.position = "right",
    legend.key = ggplot2::element_rect(fill = NA, colour = NA)
  )
}
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

make_site_flag_plot <- function(site_data, earliest_date) {
  all_pm_missing <- all(is.na(site_data$pm25))
  all_temp_missing <- all(is.na(site_data$temperature))
  all_rh_missing <- all(is.na(site_data$rh))
  if (all_pm_missing & all_temp_missing & all_rh_missing) {
    return(NULL)
  }
  plots <- list(
    site_data |> make_pm25_flag_plot(earliest_date = earliest_date),
    site_data |> make_temperature_flag_plot(earliest_date = earliest_date),
    site_data |> make_rh_flag_plot(earliest_date = earliest_date)
  )
  if (length(plots)) {
    cowplot::plot_grid(plotlist = plots, ncol = 1)
  } else {
    return(NULL)
  }
}

make_pm25_flag_plot <- function(site_data, earliest_date) {
  if (nrow(site_data) < 2) {
    return(NULL)
  }
  s_id <- site_data$site_id[1]
  plot_data <- site_data |>
    dplyr::select(-pm25) |>
    tidyr::pivot_longer(
      c("pm25_a", "pm25_b"),
      names_to = "src",
      values_to = "pm25"
    ) |>
    dplyr::mutate(
      flag = 2 *
        ifelse(src == "pm25_a", pm25_a_flag, pm25_b_flag) +
        4 * pm25_flag,
      flag = flag |>
        factor(
          c(0, 2, 4, 6),
          c(
            "Good",
            "Flagged",
            "Disagreeing",
            "Disagreeing and Flagged"
          )
        ),
      src = factor(
        src,
        c("pm25_a", "pm25_b"),
        c("A Sensor", "B Sensor")
      )
    )
  flagged_periods <- plot_data |>
    dplyr::filter(flag != "Good", !is.na(pm25)) |>
    dplyr::group_by(src, flag) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      time_step = as.numeric(date - dplyr::lag(date), unit = "mins") |>
        handyr::swap(NA, with = 10),
      window = cumsum(time_step != 10)
    ) |>
    dplyr::group_by(src, flag, window) |>
    dplyr::summarise(start = min(date), end = max(date), .groups = "drop") |>
    dplyr::mutate(
      dplyr::across(c(flag, src), as.character),
      flag = ifelse(
        flag == "Flagged",
        paste(src |> sub(pattern = " Sensor", replacement = ""), flag),
        "A/B Disagree"
      ) |>
        factor(
          levels = c("A", "B") |> paste("Flagged") |> c("A/B Disagree")
        )
    ) |>
    dplyr::distinct(flag, window, start, end)

  fills <- c(
    "A Flagged" = "#F8766D",
    "B Flagged" = "#619CFF",
    "A/B Disagree" = "grey"
  )
  colours <- c(
    "A Sensor" = "#F8766D",
    "B Sensor" = "#619CFF"
  )

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = pm25, colour = src)) +
    ggplot2::geom_rect(
      data = flagged_periods,
      ggplot2::aes(
        fill = flag,
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      alpha = 0.45
    ) +
    ggplot2::geom_line(linewidth = 0.5, na.rm = TRUE) +
    ggplot2::scale_x_datetime(
      date_breaks = "1 days",
      date_labels = "%b %d",
      expand = ggplot2::expansion(0),
      limits = c(earliest_date - lubridate::minutes(60), max(obs$date))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0.02, 0.05)),
      limits = c(0, NA)
    ) +
    ggpubr::theme_pubr(border = TRUE) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = fills) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      colour = bquote("PM"[2.5] ~ "(" * mu * "g m"^-3 * ")"),
      title = paste0(site_data$name[1], " (ID: ", s_id, ")")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 2),
      colour = ggplot2::guide_legend(order = 1)
    )
}

make_temperature_flag_plot <- function(site_data, earliest_date) {
  if (nrow(site_data) < 2) {
    return(NULL)
  }
  s_id <- site_data$site_id[1]
  plot_data <- site_data |>
    dplyr::mutate(flagged = temperature_flag > 0) |>
    dplyr::mutate(
      flag = temperature_flag |>
        factor(
          c(0, 1, 2, 4, 8, 6, 10, 12, 14),
          c(
            "Good",
            "Missing",
            "Out of Range",
            "Repeating",
            "Rapidly Changing",
            "Out of Range / Repeating",
            "Out of Range / Rapidly Changing",
            "Repeating / Rapidly Changing",
            "Out of Range / Repeating / Rapidly Changing"
          )
        )
    )
  flagged_obs <- plot_data |>
    dplyr::filter(flag != "Good", !is.na(temperature))

  site_data |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = temperature)) +
    ggplot2::geom_line(
      linewidth = 0.2,
      na.rm = TRUE,
      ggplot2::aes(colour = "Temperature")
    ) +
    ggplot2::geom_point(
      data = flagged_obs,
      ggplot2::aes(fill = stringr::str_wrap(flag, width = 14)),
      size = 1.5,
      stroke = 0.15,
      shape = 21,
      colour = "black"
    ) +
    ggplot2::scale_x_datetime(
      date_breaks = "1 days",
      date_labels = "%b %d",
      limits = c(earliest_date, max(obs$date)),
      expand = ggplot2::expansion(0)
    ) +
    # scale_y_continuous(labels = function(x) bquote(.(x)~degree*"C" )) +
    ggplot2::scale_colour_manual(values = c(Temperature = "black")) +
    ggpubr::theme_pubr() +
    ggplot2::labs(
      y = NULL,
      x = NULL,
      fill = "Values are: ",
      colour = bquote("Temperature (" * degree * "C)") #,
      # title = paste0(site_data$name[1], " (ID: ", s_id, ")")
    ) +
    # add_theme_300dpi() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 2),
      colour = ggplot2::guide_legend(order = 1)
    )
}

make_rh_flag_plot <- function(site_data, earliest_date) {
  if (nrow(site_data) < 2) {
    return(NULL)
  }
  s_id <- site_data$site_id[1]
  plot_data <- site_data |>
    dplyr::mutate(flagged = rh_flag > 0) |>
    dplyr::mutate(
      flag = rh_flag |>
        factor(
          c(0, 1, 2, 4, 8, 6, 10, 12, 14),
          c(
            "Good",
            "Missing",
            "Out of Range",
            "Repeating",
            "Rapidly Changing",
            "Out of Range / Repeating",
            "Out of Range / Rapidly Changing",
            "Repeating / Rapidly Changing",
            "Out of Range / Repeating / Rapidly Changing"
          )
        )
    )
  flagged_obs <- plot_data |>
    dplyr::filter(flag != "Good", !is.na(rh))

  site_data |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = rh)) +
    ggplot2::geom_line(
      linewidth = 0.2,
      na.rm = TRUE,
      ggplot2::aes(colour = "Humidity")
    ) +
    ggplot2::geom_point(
      data = flagged_obs,
      ggplot2::aes(fill = stringr::str_wrap(flag, width = 14)),
      size = 1.5,
      stroke = 0.15,
      shape = 21,
      colour = "black"
    ) +
    ggplot2::scale_x_datetime(
      date_breaks = "1 days",
      date_labels = "%b %d",
      limits = c(earliest_date, max(obs$date)),
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      expand = ggplot2::expansion(add = 1)
    ) +
    ggplot2::scale_colour_manual(values = c(Humidity = "black")) +
    ggpubr::theme_pubr() +
    ggplot2::labs(
      y = NULL,
      x = NULL,
      fill = "Values are: ",
      colour = "Humidity (%)" #,
      # title = paste0(site_data$name[1], " (ID: ", s_id, ")")
    ) +
    # add_theme_300dpi() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 2),
      colour = ggplot2::guide_legend(order = 1)
    )
}

# TODO: move to handyr
elementwise_mean_no_na <- function(x, y, digits = 1) {
  n <- as.integer(!is.na(x)) + as.integer(!is.na(y))
  x <- x |> handyr::swap(NA, with = 0)
  y <- y |> handyr::swap(NA, with = 0)
  ((x + y) / n) |>
    handyr::swap(Inf, with = NA) |>
    handyr::swap(NaN, with = NA) |>
    round(digits = digits)
}

make_status_map_summary <- function(obs, value_cols, flag_threshold = 0.15) {
  flag_levels <- c(
    "Good" = 0,
    "Missing" = 1,
    "Out of Range" = 2,
    "Repeating" = 4,
    "Rapidly Changing" = 8,
    "Out of Range / Repeating" = 6,
    "Out of Range / Rapidly Changing" = 10,
    "Repeating / Rapidly Changing" = 12,
    "Out of Range / Repeating / Rapidly Changing" = 14
  )
  values <- c("pm25", unname(value_cols)) |>
    c("pm25_flagged", names(value_cols))
  map_data <- obs |>
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
        c("temperature_flag", "rh_flag"),
        list(median = \(x) {
          x[x > 0] |>
            stats::median(na.rm = TRUE) |>
            factor(flag_levels, names(flag_levels))
        })
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
      is_flagged_pm = flag_group_pm != "No Detected Issue",
      is_flagged_temp = flag_group_t != "No Detected Issue",
      is_flagged_rh = flag_group_rh != "No Detected Issue",
      flags = paste(
        ifelse(pm25_a_p_flagged >= flag_threshold, "[A]", NA),
        ifelse(pm25_b_p_flagged >= flag_threshold, "[B]", NA),
        ifelse(pm25_p_flagged >= flag_threshold, "[D]", NA),
        ifelse(temperature_p_flagged >= flag_threshold, "[T]", NA),
        ifelse(rh_p_flagged >= flag_threshold, "[RH]", NA),
        sep = ", "
      ) |>
        stringr::str_remove_all(", NA|NA, |NA")
    ) |>
    dplyr::arrange(site_id) |>
    # Convert to spatial
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  # Add hover html for each point
  map_data |>
    dplyr::mutate(
      hover_pm = map_data |>
        aqsu_status_hover(
          is_bad = .data$is_flagged_pm &
            !.data$entirely_offline,
          is_missing = .data$entirely_offline,
          duration_days = duration_days,
          flag_threshold = flag_threshold
        ),
      hover_temperature = map_data |>
        aqsu_status_hover(
          is_bad = .data$is_flagged_temp &
            !.data$entirely_offline,
          is_missing = .data$entirely_offline,
          duration_days = duration_days,
          flag_threshold = flag_threshold
        ),
      hover_rh = map_data |>
        aqsu_status_hover(
          is_bad = .data$is_flagged_rh &
            !.data$entirely_offline,
          is_missing = .data$entirely_offline,
          duration_days = duration_days,
          flag_threshold = flag_threshold
        )
    )
}
