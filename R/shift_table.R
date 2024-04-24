get_worst_grade <- function(df, grade_var, group_vars) {
  df |>
    mutate(temp = case_when(
      toupper(.data[[grade_var]]) %in% c("H", "HIGH") ~ 3,
      toupper(.data[[grade_var]]) %in% c("N", "NORMAL") ~ 2,
      toupper(.data[[grade_var]]) %in% c("L", "LOW") ~ 1,
      TRUE ~ 0
    )) |>
    group_by(!!!syms(group_vars), USUBJID) |>
    arrange(.data$temp) |>
    filter(row_number() == n()) |>
    select(-temp) |>
    ungroup()
}

get_all_grades <-
  function(df,
           trt_var,
           visit_var,
           analysis_grade_var,
           base_grade_var,
           grade_var_order) {
    expand_grid(
      !!trt_var := unique(df[[trt_var]]),
      !!visit_var := intersect(
        levels(as.factor(df[[visit_var]])), unique(df[[visit_var]])
      ),
      !!base_grade_var := as.factor(c(grade_var_order, "Total"))
    ) |>
      cross_join(tibble(!!analysis_grade_var := as.factor(grade_var_order)))
  }

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
      full_join(comb_df, by = c(trt_var, group_vars[2], analysis_grade_var, base_grade_var)) |>
      mutate(across("CNT", ~ replace_na(.x, 0)))
  }

build_shift_table <-
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
        factor(.data[[base_grade_var]], levels = c(grade_var_order, "Total")),
        factor(.data[[analysis_grade_var]], levels = c(grade_var_order, "Total"))
      )
    ## pivot to get values of `base_grade_var` as columns
    grade_counts_wide <- grade_counts |>
      pivot_wider(
        id_cols = all_of(c(visit_var[1], analysis_grade_var)),
        names_from = all_of(c(trt_var, base_grade_var)),
        values_from = "CNT",
        names_sep = "<br>----------<br>Baseline<br>n (%)^"
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
      arrange(factor(.data[[visit_var[1]]], levels = visit_levels))
  }
