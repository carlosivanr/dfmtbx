#' Score Practice Culture Scale
#'
#' This function scores the 22-item practice culture scale from survey data. Input
#' data frame must be in the following order:
#' 1. After making a change, we discuss what worked and what didn't.
#' 2. My opinion is valued by others in this practice.
#' 3. People in this practice understand how their jobs fit into the rest of this practice.
#' 4. This practice is almost always in chaos.
#' 5. I can rely on the other people in this practice to do their jobs well.
#' 6. This practice puts a great deal of effort into improving the quality of care.
#' 7. This practice encourages everybody's input for making changes.
#' 8. We regularly take time to consider ways to improve how we do things.
#' 9. The practice leadership makes sure that we have the time and space necessary to discuss changes to improve care.
#' 10. This practice is very disorganized.  
#' 11. When there is conflict or tension in this practice, those involved are encouraged to talk about it.
#' 12. People in this practice are thoughtful about how they do their jobs.
#' 13. This practice uses data and information to improve the work of the practice.
#' 14. Our practice encourages people to share their ideas about how to improve things.
#' 15. People in this practice pay attention to how their actions affect others in the practice.
#' 16. The leadership in this practice is available to discuss work related problems.
#' 17. When we experience a problem in the practice we make a serious effort to figure out what's really going on.
#' 18. Our practice has recently been very stable.
#' 19. Things have been changing so fast in our practice that it is hard to keep up with what is going on.
#' 20. The leadership of this practice is good at helping us to make sense of problems or difficult situations.
#' 21. Most of the people who work in our practicee seem to enjoy their work.
#' 22. The practice leadership promotes an environment that is an enjoyable place to work.
#'
#' The practice culture is composed of the following three sub-scales that are 
#' scored as follows.
#' 1) Improvement and Change Culture
#'     - 1, 6:9, 13:14, 16:17, 20
#'     - Scoring: 25*((q1 + q6 + q7 + q8 + q9 + q13 + q14 +q16 + q17 + q20) -10)/10
#' 2) Work Relationships
#'     - 2:3, 5, 11:12, 15, 21:22
#'     - Scoring: 25*((q2 + q3 + q5 + q11 + q12 + q15 + q21 + q22) – 8) /8
#' 3) Chaos
#'     - 4, 10, 18:19
#'     - Scoring:  25*((q4 + q10 + (6-q18) + q19) - 4)/4
#'     - N.b.: Item 18 (stability) is reverse scored
#'
#' @param df A data frame containing 22 numeric columns and an identifier column.
#'  The 22 columns must be in order of how the scale was intended to be 
#'  administered
#' @param id A character vector specifying the name of the identifier column.
#'
#' @return A data frame with subscale scores and overall culture score.
#' @export
#'
#' @examples
#' data %>%
#'  select(record_id, after_change:environment) %>%
#'  score_pc_scale(., "record_id")
score_pc_scale <- function(df, id) {
  # Set the names of all columns
  all_cols <- names(df)

  # Set the names of the columns corresponding to the assessment items
  pc_items <- all_cols[!all_cols %in% id]

  # Check if the number of assessment columns is equal to 22
  n_cols <- df %>%
    select(all_of(pc_items)) %>%
    length()

  if (n_cols != 22) {
    stop("Number of scorable columns is not 22. Review input")
  }

  # Check if all assessment columns are numeric
  all_numeric <- df %>%
    select(all_of(pc_items)) %>%
    summarise(all_numeric = all(map_lgl(., is.numeric))) %>%
    pull()

  if (!all_numeric) {
    stop("Scorable columns in input data frame are not numeric. Check input column types.")
  }

  # Set the subscale items based on position to allow for different field names
  # 1. Improvement and change culture
  pc_imp_items <- pc_items[c(1, 6:9, 13:14, 16:17, 20)]
                                            
  # 2. Work relationships
  pc_rel_items <- pc_items[c(2:3, 5, 11:12, 15, 21:22)]

  # 3. Chaos Question 18 must be reversed scored
  pc_chs_items <- pc_items[c(4, 10, 18:19)]

  # Reverse score chaos subscale iterm #3 before averaging
  df <- df %>%
    mutate(!!sym(pc_chs_items[3]) := 6 - !!sym(pc_chs_items[3]))

  # na.rm FALSE will set NA for participants that did not score all items
  df <- df %>%
    mutate(
      pc_imp_subscore = 25 * (rowSums(select(., all_of(pc_imp_items)), na.rm = FALSE) - 10)/10,
      pc_rel_subscore = 25 * (rowSums(select(., all_of(pc_rel_items)), na.rm = FALSE) - 8)/8,
      pc_chs_subscore = 25 * (rowSums(select(., all_of(pc_chs_items)), na.rm = FALSE) - 4)/4,
      Chaos_rev = 100 - pc_chs_subscore,
      CultureAvg = (pc_imp_subscore + pc_rel_subscore + Chaos_rev) / 3
      )

  df <- df %>%
    select(-all_of(pc_items))


  return(df)

}