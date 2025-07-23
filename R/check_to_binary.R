#' Convert REDCap Select all that apply "label" to "binary" intended to modify 
#' input for tbl_summary() calls.
#'
#' @param df A data frame containing character columns of select all that apply
#' 
#' @param val_to_1 A character vector specifying the character value to convert to 1 
#'
#' @return A data frame with average scores
#' @export
#'
#' @examples
#' data %>%
#'  select(record_id, expect_certain_number:appreciation) %>%
#'  score_ic_scale(., "record_id")

check_to_binary <- function (df, cols = everything(), val_to_1 = "Checked") {

    # Check if all assessment columns are numeric
  all_character <- df %>%
    summarise(all_character = all(map_lgl(., is.character))) %>%
    pull()

  if (!all_character) {
    stop("Not all input columns are character type. Check input column types.")
  }


  df <- df %>%
    mutate(across({{cols}}, ~ map_int(.x, ~ ifelse(.x == val_to_1, 1, 0))))

  return(df)
}