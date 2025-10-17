#' format_gt_tbl()
#' 
#' Format a dataframe or tibble of statistical output for dispaly in a rendered
#' Quarto document. More specifically this function rounds digits and formats
#' p.values to < 0.001 if value is met.
#' 
#' 
#' @param df a dataframe or tibble of statistical output such as that from 
#'    broom::tidy()
#' @param tol a tolerance level
#' @return a gt() table
#' @export
format_gt_tbl <- function(df) {

  # Find columns with floating point values to round
  find_float_columns <- function(df, tol = .Machine$double.eps^0.5) {
    names(df)[map_lgl(df, ~ is.numeric(.x) && any(abs(.x - round(.x)) > tol, na.rm = TRUE))]
  }

  # Get the float columns
  col_names <- find_float_columns(df)

  # Find columns that have a p value to format if less than 0.001
  # Find columns where the values are numeric, must contain at least one 
  # non-missing value, and all values are between 0 and 1.
  find_pvalue_columns <- function(df) {
    names(df)[purrr::map_lgl(df, ~ {
      is.numeric(.x) &&
        any(!is.na(.x)) &&
        all(.x >= 0 & .x <= 1, na.rm = TRUE)
    })]
  }

  # Get the suspected p value columns
  p_col <- find_pvalue_columns(df)
  
  
  
  # Format the input data frame to a gt() object
  df %>%
  gt::gt() %>%
    
  # Format the columns other than the p value column to 2 digits
  gt::fmt_number(
    columns = col_names[!col_names %in% p_col],
    decimals = 2  # Use `decimals` for fixed decimal places
  ) %>%

  # Format the p value column to 4 digits
  gt::fmt_number(
    columns = p_col,
    decimals = 4  # Use `decimals` for fixed decimal places
  ) %>%

  # Format the p value column if the values is less than 0.001
  gt::fmt(
    columns = p_col,
    fns = function(x) {
      ifelse(x < 0.001, "< 0.001", formatC(x, format = "f", digits = 3))
    }
  ) %>%

  # Convert missing to blank spaces
  gt::sub_missing(
    columns = everything(),   # or specify columns like vars(col1, col2)
    missing_text = ""         # blank string to replace NA
  ) %>%

  # Bold the column names
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  )

}