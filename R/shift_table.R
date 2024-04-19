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
           source_df,
           trt_var,
           analysis_grade_var,
           base_grade_var,
           grade_var_order) {
    expand_grid(
      !!trt_var := unique(df[[trt_var]]),
      PARAM = intersect(levels(as.factor(source_df[["PARAM"]])), unique(df[["PARAM"]])),
      !!analysis_grade_var := as.factor(c(grade_var_order, "Total"))
    ) |>
      cross_join(tibble(!!base_grade_var := as.factor(grade_var_order)))
  }

summarize_grades <-
  function(df,
           comb_df,
           trt_var,
           group_vars,
           analysis_grade_var,
           base_grade_var) {
    df |>
      bind_rows(mutate(df, !!analysis_grade_var := "Total")) |>
      group_by(!!!syms(c(group_vars, base_grade_var, analysis_grade_var))) |>
      count(.data$PARAM, name = "CNT") |>
      ungroup() |>
      select(-PARAMCD) |>
      full_join(comb_df, by = c(trt_var, "PARAM", analysis_grade_var, base_grade_var)) |>
      mutate(across("CNT", ~ replace_na(.x, 0)))
  }

build_shift_table <-
  function(bds_dataset,
           trt_var,
           base_grade_var = exprs(BNRIND),
           analysis_grade_var = exprs(ANRIND),
           grade_var_order = exprs(Low, Normal, High)) {
    trt_var <- expr2var(trt_var)
    base_grade_var <- expr2var(base_grade_var)
    analysis_grade_var <- expr2var(analysis_grade_var)
    grade_var_order <- expr2var(grade_var_order)
    group_vars <- c(trt_var, "PARAMCD")
    # get highest values of `analysis_grade_var` within each Treatment and Parameter group
    wrst_grade <- bds_dataset |>
      get_worst_grade(analysis_grade_var, group_vars)
    # create a dataset {all_anrind_comb} with all possible combinations of Treatment, Parameter and
    # `analysis_grade_var`
    all_anrind_comb <- wrst_grade |>
      get_all_grades(bds_dataset, trt_var, analysis_grade_var, base_grade_var, grade_var_order)
    # get the count of parameter shift and merge with {all_anrind_comb} to preserve all combinations
    # of `analysis_grade_var`
    grade_counts_wide <- wrst_grade |>
      summarize_grades(
        all_anrind_comb,
        trt_var,
        group_vars,
        analysis_grade_var,
        base_grade_var
      ) |>
      arrange(
        .data[[trt_var]],
        .data$PARAM,
        factor(.data[[base_grade_var]], levels = grade_var_order),
        factor(.data[[analysis_grade_var]], levels = grade_var_order)
      ) |>
      ## pivot to get values of `base_grade_var` as columns
      pivot_wider(
        id_cols = all_of(c("PARAM", base_grade_var)),
        names_from = all_of(c(trt_var, analysis_grade_var)),
        values_from = "CNT",
        names_sep = "<->"
      )
    # calculating the row group total of `base_grade_var`
    base_grade_totals <- grade_counts_wide |>
      summarize(across(where(is.numeric), sum), .by = "PARAM") |>
      mutate(!!base_grade_var := "Total")
    # adding `base_grade_var` total to main data frame
    grade_counts_wide |>
      bind_rows(base_grade_totals) |>
      arrange(.data$PARAM)
  }
