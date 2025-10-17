#' get_fx_sz_tbl
#' 
#' Generate a table of detectable effect sizes given a sample size and standard
#'  deviation
#' 
#' @param alphas A numeric vector of alpha levels to 
#' @param sample_sizes A vector of numeric sample sizes to determine the 
#'  detectable effect sizes for
#' @param ratios A vector of sample size ratios to account for 
#'  attrition/unbalanced groups
#' @param std A numeric value representing the estimated standard deviation
#' @param test_type A string of either "paired" or "two.sample"
#' @return A data frame of detectable effect sizes given power = 80%.
#' @export
get_fx_sz_tbl <- function(alphas, sample_sizes, ratios, std, test_type) {
  # Validate inputs
  stopifnot(
    is.numeric(alphas), all(alphas > 0 & alphas < 1),
    is.numeric(sample_sizes), all(sample_sizes > 0),
    is.numeric(ratios), all(ratios >= 1),
    is.numeric(std), length(std) == 1, std > 0
  )

  # Generate an empty object to collect results
  tbl <- NULL
  
  # loop through alphas, sample sizes (n), and ratio of largest to smallest n
  for (i in seq_along(alphas)){
    for (j in seq_along(sample_sizes)){
      for (k in seq_along(ratios)){
      output <- 
        MESS::power_t_test(
        n = sample_sizes[j],
        delta = NULL,
        ratio = ratios[k],
        power = .8,
        sd = std, # Set this to one and calculate delta (effect size)
        type = test_type,
        sig.level = alphas[i])
      
      if (test_type == "paired" ) {
      tbl <- 
        bind_rows(tbl,
                  data.frame("Number of pairs"= output[["n"]][1],
                            #  "Subjects_per_group_T2"= ceiling(output[["n"]][1] * ratios[k]),
                             "Alpha" = alphas[i], 
                             "Power" = "80%",
                             "Effect_size" = round(output[["delta"]], 2),                             
                             "Detectable_difference" = round((std * output[["delta"]]), 2)))
      } else {
      tbl <- 
        bind_rows(tbl,
                  data.frame("N_group_1"= output[["n"]][1],
                             "N_group_2"= ceiling(output[["n"]][1] * ratios[k]),
                             "Ratio_lrg_sml" = ratios[k], 
                             "Alpha" = alphas[i], 
                             "Effect_size" = round(output[["delta"]], 2),  
                             "Power" = "80%",
                             "Detectable_difference_in_outcome" = round((std * output[["delta"]]), 2)))

      }
      }
    }
  }
  return(tbl)
}