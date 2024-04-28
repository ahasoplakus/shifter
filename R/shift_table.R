#' Create a dummy dataset to get all combonations of Baseline and Analysis
#' Range Indicators
#'
#' @param df Input dataset
#' @param trt_var Treatment Variable
#' @param visit_var Visit Variable
#' @param analysis_grade_var Analysis Range Indicator (`ANRIND`)
#' @param base_grade_var Reference Range Indiacator (`BNRIND`)
#' @param grade_var_order Sorting order of Range Indicator values
#'
#' @return `data.frame`
#'
get_all_grades <-
  function(df,
           trt_var,
           visit_var,
           analysis_grade_var,
           base_grade_var,
           grade_var_order) {
    expand_grid(
      !!trt_var := unique(df[[trt_var]]),
      !!visit_var := unique(df[[visit_var]]),
      !!base_grade_var := c(grade_var_order, "Total")
    ) |>
      cross_join(tibble(!!analysis_grade_var := grade_var_order))
  }

#' Summarize Grades by Visit
#'
#' @param df Input dataset
#' @param comb_df Dummy dataset having all combinations of Range Indicators
#' @param trt_var Treatment Variable
#' @param group_vars Grouping Variables based on which grades will be summarized
#' @param analysis_grade_var Analysis Range Indicator (`ANRIND`)
#' @param base_grade_var Reference Range Indiacator (`BNRIND`)
#'
#' @return `data.frame`
#'
summarize_grades <-
  function(df,
           comb_df,
           trt_var,
           group_vars,
           analysis_grade_var,
           base_grade_var) {
    df |>
      bind_rows(mutate(df, !!base_grade_var := "Total")) |>
      group_by(!!!syms(c(group_vars, base_grade_var, analysis_grade_var))) |>
      count(.data[[group_vars[2]]], name = "CNT") |>
      ungroup() |>
      full_join(
        comb_df,
        by = c(trt_var, group_vars[2], analysis_grade_var, base_grade_var)
      ) |>
      mutate(across("CNT", ~ replace_na(.x, 0)))
  }

#' Count Shifts by Visit
#'
#' @param bds_dataset Input analysis dataset
#' @param trt_var Treatment Variable
#' @param base_grade_var Reference Range Indiacator (`BNRIND`)
#' @param analysis_grade_var Analysis Range Indicator (`ANRIND`)
#' @param grade_var_order Sorting order of Range Indicator values
#' @param visit_var Visit variable
#'
#' @return `data.frame`
#'
count_shifts <-
  function(bds_dataset,
           trt_var,
           base_grade_var = exprs(BNRIND),
           analysis_grade_var = exprs(ANRIND),
           grade_var_order = exprs(Low, Normal, High),
           visit_var = exprs(AVISIT, AVISITN)) {
    trt_var <- expr2var(trt_var)
    base_grade_var <- expr2var(base_grade_var)
    analysis_grade_var <- expr2var(analysis_grade_var)
    grade_var_order <- expr2var(grade_var_order)
    visit_var <- expr2var(visit_var)
    group_vars <- c(trt_var, visit_var)
    # create a dataset {all_anrind_comb} with all possible
    # combinations of Treatment, Parameter and analysis_grade_var`
    all_anrind_comb <- bds_dataset |>
      get_all_grades(
        trt_var, visit_var[1],
        analysis_grade_var,
        base_grade_var,
        grade_var_order
      )
    # get the count of parameter shift and merge with {all_anrind_comb}
    # to preserve all combinations # of `analysis_grade_var`
    grade_counts <- bds_dataset |>
      summarize_grades(
        all_anrind_comb,
        trt_var,
        group_vars,
        analysis_grade_var,
        base_grade_var
      ) |>
      arrange(
        .data[[trt_var]],
        factor(.data[[base_grade_var]], levels = c(grade_var_order, "Total"))
      )
    ## pivot to get values of `base_grade_var` as columns
    grade_counts_wide <- grade_counts |>
      pivot_wider(
        id_cols = all_of(c(visit_var[1], analysis_grade_var)),
        names_from = all_of(c(trt_var, base_grade_var)),
        values_from = "CNT",
        names_sep = "^"
      )
    # calculating the row group total of `analysis_grade_var`
    post_base_grade_totals <- grade_counts_wide |>
      summarize(across(where(is.numeric), sum), .by = all_of(visit_var[1])) |>
      mutate(!!analysis_grade_var := "Total")
    visit_levels <-
      arrange(
        filter(
          grade_counts, !is.na(.data[[visit_var[2]]])
        ),
        by = .data[[visit_var[2]]]
      ) |>
      pull(.data[[visit_var[1]]]) |>
      unique()
    # adding `base_grade_var` total to main data frame
    grade_counts_wide |>
      bind_rows(post_base_grade_totals) |>
      arrange(
        factor(.data[[visit_var[1]]], levels = visit_levels),
        factor(
          .data[[analysis_grade_var]],
          levels = c(grade_var_order, "Total")
        )
      )
  }

#' Display Shift Table
#'
#' @param dataset Input dataset as `data.frame`
#' @param param Parameter Label
#' @param group_col Row Grouping Column
#' @param stub_header Stub Header
#' @param rtf_preheader RTF preheader text
#' @param title Header Title
#' @param footnote Table Footnote
#' @param sourcenote Source Note Citation
#'
#' @return `gt` table
#'
std_shift_display <-
  function(dataset,
           param = "",
           group_col = "AVISIT",
           stub_header = "Analysis Visit",
           rtf_preheader = c("Protocol: CDISCPILOT01", "Cutoff date: DDMMYYYY"),
           title = "",
           footnote = "This is a footnote",
           sourcenote = "") {
    dataset |>
      gt(groupname_col = group_col, row_group_as_column = TRUE) |>
      cols_label_with(
        columns = contains("ANRIND"), \(x) md("Reference<br>Range")
      ) |>
      tab_spanner_delim(delim = "^") |>
      text_transform(
        fn = \(x) map(x, \(y) md(paste0(y, "<br>Baseline<br>n (%)"))),
        locations = cells_column_spanners()
      ) |>
      tab_stubhead(md(stub_header)) |>
      tab_footnote(footnote = footnote) |>
      tab_header(
        preheader = md(rtf_preheader),
        title = md(title),
        subtitle = md(paste0("Parameter = ", param))
      ) |>
      tab_source_note(md(sourcenote)) |>
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
        page.orientation = "landscape",
        page.numbering = TRUE,
        page.header.use_tbl_headings = TRUE,
        page.footer.use_tbl_notes = TRUE,
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
    .gt_subtitle, .gt_footnotes, .gt_sourcenote {
      text-align: left !important;
      color: gray !important;
    }
    "
      )
  }
