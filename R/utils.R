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

# standard shift table display function
std_shift_display <-
  function(dataset,
           param,
           group_col = "AVISIT",
           trt_denom,
           title = "",
           footnote = "This is a footnote",
           stub_label = "Analysis Visit") {
    dataset |>
      gt(groupname_col = group_col, row_group_as_column = TRUE) |>
      cols_label_with(
        columns = contains("ANRIND"), \(x) md("Reference<br>Range")
      ) |>
      tab_spanner_delim(delim = "^") |>
      text_transform(
        fn = \(x) map(x, \(y) md(y)),
        locations = cells_column_spanners()
      ) |>
      text_transform(
        fn = \(x) add_pct(x, trt_denom[[1]], 1),
        locations = cells_body(columns = 3:6)
      ) |>
      text_transform(
        fn = \(x) add_pct(x, trt_denom[[2]], 1),
        locations = cells_body(columns = 7:10)
      ) |>
      text_transform(
        fn = \(x) add_pct(x, trt_denom[[3]], 1),
        locations = cells_body(columns = 11:14)
      ) |>
      tab_stubhead(md(stub_label)) |>
      tab_footnote(footnote = footnote) |>
      tab_header(
        title = md(title),
        subtitle = paste0("Parameter = ", param)
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = 2)
      ) |>
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = -c(1, 2))
      ) |>
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_column_labels(columns = -c(1, 2))
      ) |>
      tab_options(
        table.background.color = "white",
        table.font.names = "monospace-slab-serif",
        row_group.font.weight = "bold",
        column_labels.font.weight = "bold",
        heading.title.font.weight = "bold",
        heading.title.font.size = "20px",
        heading.padding = "10px",
        heading.subtitle.font.size = "14px"
      ) |>
      opt_css(
        css = "
    .gt_heading {
      border-top-style: hidden !important;
    }
    .gt_table {
      width: max-content !important;
    }
    .gt_subtitle {
      text-align: left !important;
      color: gray !important;
    }
    "
      )
  }
