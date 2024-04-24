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
  map_chr(x, ~ ifelse(as.numeric(.x) > 0, paste0(
    .x, " (", round(as.numeric(.x) / denom * 100, digits), "%)"
  ), .x))
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

glimpse_dataset <-
  function(dataset,
           display_vars = NULL,
           filter = NULL) {
    out <- dataset %>%
      filter_if(filter) %>%
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

tab_display <-
  function(dataset,
           param,
           group_col = "AVISIT",
           trt_denom,
           title = "",
           footnote = "This is a footnote",
           stub_label = "Analysis Visit") {
    dataset |>
      gt::gt(groupname_col = group_col, row_group_as_column = TRUE) |>
      gt::cols_label_with(columns = contains("ANRIND"), \(x) gt::md("Reference<br>Range")) |>
      gt::tab_spanner_delim(delim = "_") |>
      gt::text_transform(fn = \(x) map(x, \(y) gt::md(y)), locations = gt::cells_column_spanners()) |>
      gt::text_transform(
        fn = \(x) add_pct(x, trt_denom[[1]], 1),
        locations = gt::cells_body(columns = 3:6)
      ) |>
      gt::text_transform(
        fn = \(x) add_pct(x, trt_denom[[2]], 1),
        locations = gt::cells_body(columns = 7:10)
      ) |>
      gt::text_transform(
        fn = \(x) add_pct(x, trt_denom[[3]], 1),
        locations = gt::cells_body(columns = 11:14)
      ) |>
      gt::tab_stubhead(gt::md(stub_label)) |>
      gt::tab_footnote(footnote = footnote) |>
      gt::tab_header(
        title = gt::md(title),
        subtitle = paste0("Parameter: ", param)
      ) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = 2)
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_body(columns = 3:14)
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_column_labels(columns = 3:14)
      ) |>
      gt::tab_options(
        table.background.color = "white",
        table.font.names = "monospace-slab-serif",
        row_group.font.weight = "bold",
        column_labels.font.weight = "bold",
        heading.align = "left",
        heading.title.font.weight = "bold",
        heading.title.font.size = "20px",
        heading.padding = "10px",
        heading.subtitle.font.size = "14px"
      )
  }
