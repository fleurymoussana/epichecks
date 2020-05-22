#' Create PDF letters with feedback for countries. Returns an PDF document
#' for each country with summary counts of issues flagged.
#' Requires WHO IDSR data which has been prepared using the {clean_idsr} function,
#' and then run through the {missing_checker} and {threshold_checker} functions,
#' and all being stored in a list with a data frame for each country.
#'
#' @param x A list of data frames which has been processed with {clean_idsr},
#'  {missing_checker} and {threshold_checker}.
#' @param flags A list including missings and alert flags for each country, produced
#' by {country_feedback}
#' @param current_year The year of interest as numeric (e.g. the default is 2020)
#' @param current_week The week of interest as numeric. The default is 1.
#' Values less than 10 are padded with leading zeros, e.g. 1 becomes 01. (You
#' could also enter 01 and it would be fine)
#' @param output_path File path where you would like outputs to be saved to.
#' Within this folder a new folder will be created for the current year, and
#' within that the current week.
#' This defaults to your current directory, with subfolders of
#' Data Files > Output > 2020 > 01.
#' The year folder needs to exist already - only the current week folder will be created.
#'
#' @importFrom aweek as.aweek
#' @importFrom stringr str_pad str_glue
#' @importFrom here here
#' @importFrom rmarkdown render
#'
#' @seealso \code{\link{clean_idsr}} for preparing IDSR data for use with threshold_checker,
#' \code{\link{missing_checker}} for missing flags and \code{\link{threshold_checker}}
#' for threshold flags.
#'
#' @export
country_letters <- function(x,
                            flags = flags,
                            current_year = 2020,
                            current_week = 1,
                            output_path  = here::here("Data Files", "Output")
                            ) {

  ## pad the current week so it is always two values long
  current_week <- str_pad({current_week}, 2, pad = 0)

  ## set the current epiweek
  current_epiweek <- aweek::as.aweek(str_glue("{current_year}-W{current_week}"))

  ## pull together file path for the current week
  week_path <-  str_glue(output_path, "/",
                         current_year, "/",
                         current_week)

  ## pull the two datasets out of the list
  message_list <- flags$message_list
  alert_list <- flags$alert_list


  ## for each of the countries in processed data
  for (i in names(x)) {

    ## use the rmarkdown template and save a pdf in the appropriate week folder
    rmarkdown::render(input = system.file("extdata", "Country_letter.Rmd", package = "epichecks"),
                      output_format = "pdf_document",
                      output_file = str_glue(i, "_", current_epiweek, ".pdf"),
                      output_dir = week_path)
  }
}
