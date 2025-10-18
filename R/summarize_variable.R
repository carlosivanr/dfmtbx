#' summarize_variable()
#' 
#' Summarize a numeric variable. Calculates the non-missing, min, max, median,
#' mean, sd, and se of a numeric variable.
#' 
#' 
#' @param df a dataframe or tibble
#' @param column a variable name in the input data frame, no quotes needed
#' @return a tibble
#' @examples
#' # Basic example usage
#' summarize_variable(heights_df, height)
#' 
#' @export
summarize_variable <- function(df, var_name) {

  # Test to determine of the input variable is numeric
  num_var <- df %>%
    ungroup() %>%
    pull({{ var_name }}) %>%
    is.numeric()

  # Summarize variable
  if (num_var == TRUE) {  
    df %>%
      summarise(
        n = sum(!is.na({{ var_name }})),
        min = min({{ var_name }}, na.rm = TRUE),
        max = max({{ var_name }}, na.rm = TRUE),
        median = median({{ var_name }}, na.rm = TRUE),
        mean = mean({{ var_name }}, na.rm = TRUE),
        sd = sd({{ var_name }}, na.rm = TRUE),
        se = sd({{ var_name }}, na.rm = TRUE) / sqrt(n),
        .groups = "drop"
      )
    
  } else {

    stop("Summarize column requires a numeric variable.")

  }
}