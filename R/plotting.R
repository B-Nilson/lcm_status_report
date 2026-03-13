
make_site_flag_plot <- function(site_data, earliest_date) {
  all_pm_missing <- all(is.na(site_data$pm25))
  all_temp_missing <- all(is.na(site_data$temperature))
  all_rh_missing <- all(is.na(site_data$rh))
  all_missing <- all_pm_missing & all_temp_missing & all_rh_missing
  if (all_missing) {
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