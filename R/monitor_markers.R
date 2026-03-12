add_monitor_markers <- function(map, map_data, duration_days, popup_width_px) {
  totals <- list(
    pm = map_data$flag_group_pm |>
      levels() |>
      sapply(\(fg) dplyr::filter(map_data, flag_group_pm == fg) |> nrow()),
    temp = map_data$flag_group_t |>
      levels() |>
      sapply(\(fg) dplyr::filter(map_data, flag_group_t == fg) |> nrow()),
    rh = map_data$flag_group_rh |>
      levels() |>
      sapply(\(fg) dplyr::filter(map_data, flag_group_rh == fg) |> nrow())
  )
  map |>
    # Add flagged PM2.5 markers on top of unflagged + legend
    addAQSUStatusMarkers(
      dat = dplyr::filter(map_data, !is_flagged_pm | entirely_offline),
      totals = totals,
      flagged = FALSE,
      sensor = "PM",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    addAQSUStatusMarkers(
      dat = dplyr::filter(map_data, is_flagged_pm & !entirely_offline),
      totals = totals,
      flagged = TRUE,
      sensor = "PM",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    # Add flagged T markers on top of unflagged + legend
    addAQSUStatusMarkers(
      dat = dplyr::filter(map_data, !is_flagged_temp | entirely_offline),
      totals = totals,
      flagged = FALSE,
      sensor = "T",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    addAQSUStatusMarkers(
      dat = dplyr::filter(map_data, is_flagged_temp & !entirely_offline),
      totals = totals,
      flagged = TRUE,
      sensor = "T",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    # Add flagged RH markers on top of unflagged + legend
    addAQSUStatusMarkers(
      dat = dplyr::filter(map_data, !is_flagged_rh | entirely_offline),
      totals = totals,
      flagged = FALSE,
      sensor = "RH",
      duration_days = duration_days,
      popup_width = popup_width_px
    ) |>
    addAQSUStatusMarkers(
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

addAQSUStatusMarkers <- function(
  map,
  dat,
  totals,
  flagged = FALSE,
  sensor = c("PM", "T", "RH"),
  save_figs = TRUE,
  duration_days = 14,
  popup_width = 700 # pixels
) {
  if (nrow(dat) == 0) {
    return(map)
  }
  labelOptions_inter <- leaflet::labelOptions(
    style = list("font-family" = "Inter", "font-size" = "12px")
  )
  if (sensor == "PM") {
    dat$label = dat$hover_pm
    flag_groups <- dat$flag_group_pm |> levels()
    flag_group_labels <- flag_groups |>
      paste0(" (n= ", totals$pm, ")") # TODO: pass via arg
    pal <- leaflet::colorFactor(
      levels = flag_groups,
      ordered = TRUE,
      palette = c(
        wesanderson::wes_palette("Royal1")[1], # no data
        wesanderson::wes_palette("Darjeeling1")[2], # no issue
        wesanderson::wes_palette("FantasticFox1")[c(3, 2, 4, 5)]
      )
    )
    fills <- pal(dat$flag_group_pm)
    group <- "PM2.5 Sensors"
    leg_sizes <- c(4, 3, 5, 5, 5, 5) * 2
    current_domain <- c(0, 100)
    current_val_pal <- leaflet::colorNumeric("viridis", domain = current_domain)
    current_fills <- current_val_pal(
      dat$current_pm25 |> handyr::clamp(range = current_domain)
    )
    current_group = "Current PM2.5"
    current_val_col <- "current_pm25"
  } else if (sensor == "T") {
    dat$label = dat$hover_temperature
    flag_groups <- dat$flag_group_t |> levels()
    flag_group_labels <- flag_groups |>
      paste0(" (n= ", totals$temp, ")")
    pal <- leaflet::colorFactor(
      levels = flag_groups,
      ordered = TRUE,
      palette = c(
        wesanderson::wes_palette("Royal1")[1],
        wesanderson::wes_palette("Darjeeling1")[2],
        wesanderson::wes_palette("FantasticFox1")[c(2, 4, 5)]
      )
    )
    fills <- pal(dat$flag_group_t)
    group <- "Temperature Sensor"
    leg_sizes <- c(4, 3, 5, 5, 5) * 2
    current_domain <- c(-30, 30)
    current_val_pal <- leaflet::colorNumeric("turbo", domain = current_domain)
    current_fills <- current_val_pal(
      dat$current_temperature |> handyr::clamp(range = current_domain)
    )
    current_group = "Current Temperature"
    current_val_col <- "current_temperature"
  } else if (sensor == "RH") {
    dat$label = dat$hover_rh
    flag_groups <- dat$flag_group_rh |> levels()
    flag_group_labels <- flag_groups |>
      paste0(" (n= ", totals$rh, ")")
    pal <- leaflet::colorFactor(
      levels = flag_groups,
      ordered = TRUE,
      palette = c(
        wesanderson::wes_palette("Royal1")[1],
        wesanderson::wes_palette("Darjeeling1")[2],
        wesanderson::wes_palette("FantasticFox1")[c(2, 4, 5)]
      )
    )
    fills <- pal(dat$flag_group_rh)
    group <- "Humidity Sensor"
    leg_sizes <- c(4, 3, 5, 5, 5) * 2
    current_domain <- c(0, 100)
    current_val_pal <- leaflet::colorNumeric(
      "turbo",
      reverse = TRUE,
      domain = current_domain
    )
    current_fills <- current_val_pal(
      dat$current_rh |> handyr::clamp(range = current_domain)
    )
    current_group <- "Current RH"
    current_val_col <- "current_rh"
  } else {
    stop("Invalid input for 'sensor' - must be either 'T', 'RH', or 'PM'")
  }

  map <- map |>
    leaflet::addCircleMarkers(
      data = dat,
      group = group,
      radius = ~ dplyr::case_when(
        entirely_offline ~ 5,
        flagged ~ 7,
        TRUE ~ 3
      ),
      weight = 1,
      color = "black",
      fillColor = fills,
      fillOpacity = 1,
      label = ~ lapply(label, htmltools::HTML),
      labelOptions = labelOptions_inter,
      popup = ~popup,
      popupOptions = leaflet::popupOptions(minWidth = popup_width)
    ) |>
    leaflet::addCircleMarkers(
      data = dat,
      group = current_group,
      radius = ~ dplyr::case_when(
        is.na(get(current_val_col)) ~ 3,
        # flagged ~ 7,
        TRUE ~ 5
      ),
      weight = 1,
      color = "black",
      fillColor = current_fills,
      fillOpacity = 1,
      label = ~ lapply(label, htmltools::HTML),
      labelOptions = labelOptions_inter,
      popup = ~popup,
      popupOptions = leaflet::popupOptions(minWidth = popup_width)
    )
  # Add legend only once (addAQSUStatusMarkers is called seperately for flagged/unflagged)
  if (!flagged) {
    map <- map |>
      addLegendCustom(
        colors = pal(flag_groups),
        labels = flag_group_labels,
        group = group,
        sizes = leg_sizes,
        opacity = 1,
        title = paste(
          "Most common status<br>for the past",
          duration_days,
          "days"
        )
      ) |>
      leaflet::addLegend(
        pal = current_val_pal,
        values = dat[[current_val_col]] |> c(current_domain) |> na.omit(),
        group = current_group,
        opacity = 1,
        title = "Current Value"
      )
  }
  return(map)
}
