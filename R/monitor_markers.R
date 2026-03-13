add_monitor_markers <- function(map, map_data, duration_days, popup_width_px) {
  totals <- list(
    PM = map_data$flag_group_pm |>
      levels() |>
      sapply(\(fg) dplyr::filter(map_data, flag_group_pm == fg) |> nrow()),
    T = map_data$flag_group_t |>
      levels() |>
      sapply(\(fg) dplyr::filter(map_data, flag_group_t == fg) |> nrow()),
    RH = map_data$flag_group_rh |>
      levels() |>
      sapply(\(fg) dplyr::filter(map_data, flag_group_rh == fg) |> nrow())
  )
  map |>
    # Add flagged PM2.5 markers on top of unflagged + legend
    add_monitor_network_markers(
      dat = dplyr::filter(map_data, !is_flagged_pm | entirely_offline),
      totals = totals,
      flagged = FALSE,
      sensor = "PM",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    add_monitor_network_markers(
      dat = dplyr::filter(map_data, is_flagged_pm & !entirely_offline),
      totals = totals,
      flagged = TRUE,
      sensor = "PM",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    # Add flagged T markers on top of unflagged + legend
    add_monitor_network_markers(
      dat = dplyr::filter(map_data, !is_flagged_temp | entirely_offline),
      totals = totals,
      flagged = FALSE,
      sensor = "T",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    add_monitor_network_markers(
      dat = dplyr::filter(map_data, is_flagged_temp & !entirely_offline),
      totals = totals,
      flagged = TRUE,
      sensor = "T",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    # Add flagged RH markers on top of unflagged + legend
    add_monitor_network_markers(
      dat = dplyr::filter(map_data, !is_flagged_rh | entirely_offline),
      totals = totals,
      flagged = FALSE,
      sensor = "RH",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    add_monitor_network_markers(
      dat = dplyr::filter(map_data, is_flagged_rh & !entirely_offline),
      totals = totals,
      flagged = TRUE,
      sensor = "RH",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    # Hide T/RH sensor and Current reading markers to start
    leaflet::hideGroup("Temperature Sensor") |>
    leaflet::hideGroup("Humidity Sensor") |>
    leaflet::hideGroup("Current PM2.5") |>
    leaflet::hideGroup("Current Temperature") |>
    leaflet::hideGroup("Current RH")
}

add_monitor_network_markers <- function(
  map,
  dat,
  totals,
  flagged = FALSE,
  sensor = c("PM", "T", "RH"),
  save_figs = TRUE,
  duration_days = 14,
  popup_width = 700 # pixels
) {
  stopifnot(sensor %in% c("PM", "T", "RH"))
  if (nrow(dat) == 0) {
    return(map)
  }

  # Define layer / column names
  layer_names <- list(
    flag = list(
      PM = "PM2.5 Sensors",
      T = "Temperature Sensor",
      RH = "Humidity Sensor"
    ),
    values = list(
      PM = "Current PM2.5",
      T = "Current Temperature",
      RH = "Current RH"
    )
  )
  sensors_long <- list(PM = "pm25", T = "temperature", RH = "rh")
  value_columns <- sensors_long |> lapply(\(x) paste0("current_", x))
  hover_columns <- sensors_long |> lapply(\(x) paste0("hover_", x))
  flag_columns <- paste0("flag_group_", tolower(names(sensors_long))) |>
    as.list() |>
    setNames(names(sensors_long))

  # Define colour palettes
  value_domains <- list(
    PM = c(0, 100),
    T = c(-30, 30),
    RH = c(0, 100)
  )
  palettes <- dat |>
    make_marker_palettes(
      value_domains = value_domains,
      flag_columns = flag_columns
    )

  # Apply palettes to sensor flag and value columns
  groups <- c(layer_names$flag[[sensor]], layer_names$values[[sensor]])
  dat <- dat |>
    dplyr::mutate(
      flag_fills = palettes$flag[[sensor]](get(flag_columns[[sensor]])),
      flag_radius = dplyr::case_when(
        entirely_offline ~ 5,
        flagged ~ 7,
        .default = 3
      ),
      value_fills = get(value_columns[[sensor]]) |>
        handyr::clamp(range = value_domains[[sensor]]) |>
        palettes$values[[sensor]](),
      value_radius = dplyr::case_when(
        is.na(get(value_columns[[sensor]])) ~ 3,
        .default = 5
      ),
      label = lapply(get(hover_columns[[sensor]]), htmltools::HTML)
    )

  popup_options <- leaflet::popupOptions(minWidth = popup_width)
  map <- map |>
    leaflet::addCircleMarkers(
      data = dat,
      group = groups[1],
      radius = ~flag_radius,
      weight = 1,
      color = "black",
      fillColor = ~flag_fills,
      fillOpacity = 1,
      label = ~label,
      popup = ~popup,
      popupOptions = popup_options
    ) |>
    leaflet::addCircleMarkers(
      data = dat,
      group = groups[2],
      radius = ~value_radius,
      weight = 1,
      color = "black",
      fillColor = ~value_fills,
      fillOpacity = 1,
      label = ~label,
      popup = ~popup,
      popupOptions = popup_options
    )
  # Add legend only once (addAQSUStatusMarkers is called seperately for flagged/unflagged)
  if (!flagged) {
    map <- map |>
      add_marker_legends(
        dat = dat,
        sensor = sensor,
        groups = groups,
        palettes = palettes,
        flag_columns = flag_columns,
        value_columns = value_columns,
        totals = totals,
        value_domains = value_domains,
        duration_days = duration_days
      )
  }
  return(map)
}

make_marker_palettes <- function(dat, value_domains, flag_columns) {
  main_colours <- c(
    wesanderson::wes_palette("Royal1")[1],
    wesanderson::wes_palette("Darjeeling1")[2],
    wesanderson::wes_palette("FantasticFox1")[c(3, 2, 4, 5)]
  )
  palettes <- list(
    flag = list(
      PM = main_colours,
      T = main_colours[-3],
      RH = main_colours[-3]
    ),
    values = list(
      PM = leaflet::colorNumeric("viridis", domain = value_domains$PM),
      T = leaflet::colorNumeric("turbo", domain = value_domains$T),
      RH = leaflet::colorNumeric(
        "turbo",
        reverse = TRUE,
        domain = value_domains$RH
      )
    )
  )
  # flag colours -> palette functions
  palettes$flag <- names(palettes$flag) |>
    setNames(names(palettes$flag)) |>
    lapply(\(pal_sensor) {
      colours <- palettes$flag[[pal_sensor]]
      flag_col <- flag_columns[[pal_sensor]]
      flag_groups <- dat[[flag_col]] |> levels()
      leaflet::colorFactor(
        levels = flag_groups,
        ordered = TRUE,
        palette = colours,
        domain = dat[[flag_col]]
      )
    })
  return(palettes)
}

add_marker_legends <- function(
  map,
  dat,
  sensor,
  groups,
  palettes,
  flag_columns,
  value_columns,
  totals,
  value_domains,
  duration_days
) {
  flag_groups <- dat[[flag_columns[[sensor]]]] |> levels()
  flag_group_labels <- flag_groups |>
    paste0(" (n= ", totals[[sensor]], ")")
  legend_sizes <- list(
    PM = c(4, 3, 5, 5, 5, 5) * 2,
    T = c(4, 3, 5, 5, 5) * 2,
    RH = c(4, 3, 5, 5, 5) * 2
  )
  value_units <- list(PM = " µg m⁻³", T = " °C", RH = "%")
  map <- map |>
    add_legend_custom(
      colours = palettes$flag[[sensor]](flag_groups),
      labels = flag_group_labels,
      group = groups[1],
      sizes = legend_sizes[[sensor]],
      opacity = 1,
      title = "Most Common %s<br>Status for the Past %s Days" |>
        sprintf(groups[1], duration_days)
    ) |>
    leaflet::addLegend(
      pal = palettes$values[[sensor]],
      values = dat[[value_columns[[sensor]]]] |>
        handyr::clamp(value_domains[[sensor]]) |>
        c(value_domains[[sensor]]) |>
        na.omit(),
      labFormat = leaflet::labelFormat(suffix = value_units[[sensor]]),
      group = groups[2],
      opacity = 1,
      title = groups[2] |>
        sub(pattern = "Temperature", replacement = "Temp.", fixed = TRUE)
    )
}

add_legend_custom <- function(
  map,
  colours,
  labels,
  sizes = rep(10, length(colours)),
  colour_class = "custom-legend-colour",
  label_class = "custom-legend-label",
  ...
) {
  # Adjust margins to center smaller sizes
  normal_margin <- 4
  margin_offsets <- (max(sizes) - sizes)
  margins <- "margin-left: %spx; margin-right: %spx" |>
    sprintf(
      floor(margin_offsets / 2),
      normal_margin + ceiling(margin_offsets / 2)
    )
  margins <- ifelse(margin_offsets == 0, "", margins)

  sizes <- paste0(sizes, "px")
  colours <- "%s; width: %s; height: %s; %s\" class=\"%s\"" |>
    sprintf(colours, sizes, sizes, margins, colour_class)
  labels <- "<div class=\"%s\" style=\"line-height: %s;\">%s</div>" |>
    sprintf(label_class, sizes, labels)

  map |>
    leaflet::addLegend(colors = colours, labels = labels, ...)
}
