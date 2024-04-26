# convert expressions to vectors
expr2var <- function(expressions) {
  set_names(map_chr(expressions, as_string), names(expressions))
}

# replace `NA_character_` values with "<Missing>"
na_to_missing <- function(df) {
  df |>
    mutate(across(
      where(~ is.factor(.x) || is.character(.x)),
      \(x) {
        case_match(
          str_squish(x),
          "" ~ "<Missing>",
          NA_character_ ~ "<Missing>",
          .default = x
        )
      }
    ))
}

# get treatment totals
get_trt_denom <- function(adsl, trt_var, trt_val) {
  adsl |>
    select(-all_of(c("USUBJID"))) |>
    distinct() |>
    filter(.data[[trt_var]] == trt_val) |>
    pull()
}

# glue percentages to numeric values
add_pct <- function(x, denom, digits = 2) {
  map_chr(x, ~ ifelse(as.numeric(.x) > 0, paste0(
    .x, " (", round(as.numeric(.x) / denom * 100, digits), "%)"
  ), .x))
}

# view intermediate datasets with `DT` extensions
glimpse_dataset <-
  function(dataset,
           display_vars = NULL) {
    out <- dataset |>
      mutate(across(where(is.character), as.factor))

    if (!is.null(display_vars)) {
      hide_columns <- which(!(colnames(out) %in% expr2var(display_vars)))
      cols_to_hide <-
        list(list(targets = hide_columns - 1, visible = FALSE))
    } else {
      cols_to_hide <- list()
    }

    DT::datatable(
      out,
      rownames = FALSE,
      filter = "top",
      height = "auto",
      width = "auto",
      extensions = c("Buttons", "ColReorder", "Scroller"),
      options = list(
        columnDefs = cols_to_hide,
        searchHighlight = TRUE,
        searching = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20, 50, 100),
        dom = "<Bfr<\"dt-scroll\"t>ipl>",
        buttons = list(
          list(
            extend = "colvis",
            text = "View",
            scroller = TRUE,
            collectionLayout = "four-column"
          )
        ),
        colReorder = TRUE
      )
    )
  }
