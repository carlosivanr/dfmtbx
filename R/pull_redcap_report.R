#' pull_redcap_report
#'
#' Pulls a report from a RedCap project given a project token, report id, and a
#' value type. Requires a pre-defined set of fields specified in a RedCap
#' project report and API access to the RedCap project.
#'
#' @param token A RedCap API token as a string corresponding to the RedCap 
#'  project of interest. The token can be saved as an environmental variable
#'  in Windows and retrieved via Sys.getenv("environmental variable name") to
#'  prevent uploading the token to GitHub.
#' @param report_id A numeric report id as a string that corresponds to a given
#'  report in the RedCap project of interest
#' @param value_type Options are "label" or "raw". "label" returns the textual
#'  version of the RedCap field responses such as "5 - Strongly agree" or 
#'  "Male" for fixed responses and "Checked"/"Unchecked" for select all that
#'  apply. "raw" returns numeric values such as 5, 
#' @param header_type Options are "label" or "raw". "raw" pulls the field names
#'  as found in the RedCap project, while "label" pulls the questions that 
#'  correspond to the field eg. "Rate on a scale of 1-10 your level of pain."
#' @param t_stamps Options are "true" or "false". "true" pulls the time stamps
#' for variables that indicate a given form is complete.
#' @return a dataframe
#' @examples
#' # Basic example usage
#' data <- pull_redcap_report(Sys.getenv("LC_patient"), "81729739", "label", "raw", "true", "false")
#'  where "LC_patient" is a Windows environmental variable containing the 
#'  RedCap project API token.
#' 
#' @export
pull_redcap_report <- function(token, report_id, value_type, header_type, t_stamps) {
  .token <- token

  url <- "https://redcap.ucdenver.edu/api/"
  
  formData <- list(
    token = .token, 
    content = "report", 
    format = "csv",
    report_id = report_id, 
    csvDelimiter = "", 
    rawOrLabel = value_type,
    rawOrLabelHeaders = header_type,     
    exportCheckboxLabel = "false",
    exportSurveyFields = t_stamps,
    returnFormat = "csv")
  
  response <- httr::POST(url, body = formData, encode = "form")
  
  temp <- httr::content(response, show_col_types = FALSE)
  
  return(temp)
}