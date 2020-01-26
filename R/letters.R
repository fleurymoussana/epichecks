#' Create PDF letters with feedback for countries. Returns an PDF document
#' for each country with summary counts of issues flagged.
#' Requires WHO IDSR data which has been prepared using the {clean_data} function,
#' and then run through the {missing_checker} and {threshold_checker} functions,
#' and all being stored in a list with a data frame for each country.
#'
#' @param x A list of data frames which has been processed with {clean_data},
#'  {missing_checker} and {threshold_checker}.
#' @param flags A list including missings and alert flags for each country, produced
#' by {country_feedback}
#' @param current_week The week of interest as a character or {aweek} object.
#' Needs to be in the correct format ("YYYY-Www").
#' The default "2018-W35" is just to demonstrate the necessary format.
#' @param output_path File path where you would like outputs to be saved to.
#' Within this folder a new folder will be created and named after the current week.
#' This defaults to your current directory, with subfolders of data > outputs > verification.
#' These folders need to exist already - only the current week folder will be created.
#'
#' @importFrom aweek as.aweek
#' @importFrom stringr str_glue
#' @importFrom here here
#' @importFrom rmarkdown render
#'
#' @seealso \code{\link{clean_data}} for preparing IDSR data for use with threshold_checker,
#' \code{\link{missing_checker}} for missing flags and \code{\link{threshold_checker}}
#' for threshold flags.
#'
#' @export
country_letters <- function(x,
                            flags = flags,
                            current_week = "2018-W35",
                            output_path  = here::here("Data", "Outputs", "Verification")
                            ) {

  ## make current_week an {aweek} object
  current_week <- aweek::as.aweek(current_week)

  ## pull together file path for the current week
  week_path <- str_glue(output_path, "/", current_week)

  ## pull the two datasets out of the list
  message_list <- flags$message_list
  alert_list <- flags$alert_list


  ## for each of the countries in processed data
  for (i in names(x)) {

    ## use the rmarkdown template and save a pdf in the appropriate week folder
    rmarkdown::render(input = system.file("extdata", "Country_letter.Rmd", package = "epichecks"),
                      output_format = "pdf_document",
                      output_file = str_glue(i, "_", current_week, ".pdf"),
                      output_dir = week_path)
  }
}
