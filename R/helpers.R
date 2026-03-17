unite_w_newline <- function(x, cols = dplyr::everything(), newline = "<br>") {
  x |>
    as.data.frame() |>
    tidyr::unite(
      {{ cols }},
      col = "summary",
      sep = newline,
      na.rm = TRUE
    ) |>
    dplyr::pull("summary")
}

most_common <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))][1]
}
