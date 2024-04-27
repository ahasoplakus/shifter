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
get_trt_total <- function(adsl, trt_vars, trt_val) {
  trt_vars <- expr2var(trt_vars)
  adsl |>
    select(all_of(c(trt_vars))) |>
    distinct() |>
    filter(.data[[trt_vars[1]]] == trt_val) |>
    pull()
}

# calculate percentages
add_pct <- function(x, denom, digits = 1) {
  if_else(x > 0, paste0(x, " (", round(x / denom * 100, digits), "%)"), as.character(x))
}

# glue percentages to numeric values within cells
num_to_pct <- function(dataset, denom, digits = 1) {
  bind_cols(map(names(denom), \(cols) {
    dataset |>
      select(matches(cols)) |>
      mutate(across(everything(), \(y) add_pct(y, denom[[cols]], digits)))
  }))
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
