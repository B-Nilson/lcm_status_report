make_site_flag_plot <- function(
  site_data,
  date_range = NULL,
  types = c("pm25", "temperature", "rh")
) {
  if (is.null(date_range)) {
    date_range <- range(site_data$date)
  }
  types |>
    lapply(\(type) {
      gg <- site_data |> make_flag_plot(type = type, date_range = date_range)
      if (type != dplyr::first(types)) {
        gg <- gg + ggplot2::labs(title = NULL)
      }
      if (type != dplyr::last(types)) {
        gg <- gg + ggplot2::labs(caption = NULL)
      }
      return(gg)
    }) |>
    patchwork::wrap_plots(plotlist = _, ncol = 1)
}

make_flag_plot <- function(site_data, date_range, type = "pm25") {
  fills <- c(
    # PM2.5 ----------------------
    "A bad" = "#F8766D",
    "B bad" = "#619CFF",
    "A bad, B bad" = "#AD89B6",
    "AB disagree" = "grey",
    # Temperature & RH -----------
    "Out of range" = "#F8766D",
    "Spiking" = "#619CFF",
    "Repeating" = "#93AA00"
  )
  colours <- c(
    # PM2.5 ----------------------
    "A sensor" = "#F8766D",
    "B sensor" = "#619CFF",
    # Temperature & RH -----------
    " " = "black" # display no label (rely on legend title)
  )

  if (type == "pm25") {
    plot_data <- site_data |>
      dplyr::select(-pm25) |>
      tidyr::pivot_longer(
        c("pm25_a", "pm25_b"),
        names_to = "src",
        values_to = "pm25",
        names_transform = \(x) {
          sub("pm25_", "", x) |> toupper() |> paste("sensor")
        }
      )
  } else {
    plot_data <- site_data |>
      dplyr::mutate(src = " ") # display no label (rely on legend title)
  }
  plot_data <- plot_data |>
    dplyr::rename_with(
      \(x) ifelse(x == type, "value", "flag"),
      .cols = dplyr::ends_with("flag_name") & dplyr::starts_with(type) | !!type
    )

  base_plot <- plot_data |> make_base_plot(date_range = date_range, type = type)
  if (nrow(plot_data) < 2) {
    return(base_plot)
  }

  needs_flags <- any(complete.cases(plot_data$flag, plot_data$value))
  if (needs_flags) {
    flagged_periods <- plot_data |> get_flagged_periods()
    base_plot <- base_plot +
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
      )
  }

  base_plot +
    ggplot2::geom_line(linewidth = 0.5, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = fills, breaks = names(fills))
}

make_base_plot <- function(plot_data, date_range, type = "pm25") {
  plot_title <- "%s (PurpleAir ID: %s)" |>
    sprintf(plot_data$name[1], plot_data$site_id[1])

  legend_titles <- list(
    pm25 = bquote("PM"[2.5] ~ "(" * mu * "g m"^-3 * ")"),
    temperature = bquote("Temperature (" * degree * "C)"),
    rh = "Humidity (%)"
  )

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = value, colour = src)) +
    ggplot2::scale_x_datetime(
      date_breaks = "1 days",
      date_labels = "%b %d",
      expand = ggplot2::expansion(0),
      limits = date_range
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0.02, 0.05)),
      limits = c(ifelse(type == "temperature", NA, 0), NA)
    ) +
    ggpubr::theme_pubr(border = TRUE) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      colour = legend_titles[[type]],
      caption = "10-minute average data from %s to %s (UTC)" |>
        sprintf(
          min(plot_data$date),
          max(plot_data$date)
        ),
      title = plot_title
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 2),
      colour = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Inter"),
      legend.text = ggplot2::element_text(size = 12), # increase font size
      legend.margin = ggplot2::margin(0, 1, 1, 1), # Reduce spacing between legends
      legend.box.spacing = ggplot2::unit(6, "pt"), # reduce spacing between plot and legend
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
}

get_flagged_periods <- function(flag_plot_data) {
  flag_plot_data |>
    dplyr::filter(complete.cases(flag, value)) |>
    dplyr::group_by(src, flag) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      time_step = as.numeric(date - dplyr::lag(date), unit = "mins") |>
        handyr::swap(NA, with = 10),
      window = cumsum(time_step != 10)
    ) |>
    dplyr::group_by(src, flag, window) |>
    dplyr::summarise(start = min(date), end = max(date), .groups = "drop") |>
    dplyr::distinct(flag, window, start, end)
}
