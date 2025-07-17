#' Score Implementation Climate Scale
#'
#' @param df A data frame containing 6 numeric columns and an identifier column.
#' 
#' @param id A character vector specifying the name of the identifier column.
#'
#' @return A data frame with average scores
#' @export
#'
#' @examples
#' data %>%
#'  select(record_id, expect_certain_number:appreciation) %>%
#'  score_ic_scale(., "record_id")

score_ic_scale <- function(df, id) {
  # Set the names of all columns
  all_cols <- names(df)

  # Set the names of the columns corresponding to the assessment items
  ic_scale_items <- all_cols[!all_cols %in% id]

  # Check if the number of assessment columns is equal to 22
  n_cols <- df %>%
    select(all_of(ic_scale_items)) %>%
    length()

  if (n_cols != 6) {
    stop("Number of scorable columns is not 6. Review input")
  }

  # Check if all assessment columns are numeric
  all_numeric <- df %>%
    select(all_of(ic_scale_items)) %>%
    summarise(all_numeric = all(map_lgl(., is.numeric))) %>%
    pull()

  if (!all_numeric) {
    stop("Scorable columns in input data frame are not numeric. Check input column types.")
  }

  # Calculate the average of the implementation climate scale items
  df <- df %>%
    mutate(AvgImplScale = rowMeans(select(., all_of(ic_scale_items))))
     
  df <- df %>%
    select(-all_of(ic_scale_items))

  return(df)

}