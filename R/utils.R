expr2var <- function(expressions) {
  set_names(map_chr(expressions, as_string), names(expressions))
}

na_to_missing <- function(df) {
  df |>
    mutate(across(
      where(~ is.factor(.x) || is.character(.x)),
      \(x) case_match(
        str_squish(x),
        "" ~ "<Missing>",
        NA_character_ ~ "<Missing>",
        .default = x
      )
    ))
}

filter_if <- function(df, filter) {
  if (is.null(filter)) {
    df
  } else {
    filter(df, !!filter)
  }
}

get_trt_N <- function(adsl, trt_var, trt_val) {
  adsl |>
    select(-USUBJID) |>
    distinct() |>
    filter(.data[[trt_var]] == trt_val) |>
    pull()
}

custom_split <- function(string, pattern) {
  map_chr(str_split(string, pattern), \(x) x[2])
}

add_pct <- function(x, denom, digits = 2) {
  paste0(x, " (", round(as.numeric(x) / denom * 100, digits), ")")
}

gt_pal <- function() {
  c(
    "#FFFFFF",
    "#FFF5F0",
    "#FEE0D2",
    "#FCBBA1",
    "#FC9272",
    "#FB6A4A",
    "#EF3B2C",
    "#CB181D",
    "#A50F15"
  )
}