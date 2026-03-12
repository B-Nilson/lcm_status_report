add_search_menu <- function(
  map,
  target_groups = "PM2.5 Sensors",
  search_property = "label"
) {
  search_options <- leaflet.extras::searchFeaturesOptions(
    propertyName = search_property,
    zoom = 12,
    openPopup = TRUE,
    firstTipSubmit = TRUE,
    autoCollapse = TRUE,
    hideMarkerOnCollapse = TRUE
  )

  map |>
    leaflet.extras::addSearchFeatures(
      targetGroups = target_groups,
      options = search_options
    )
}
