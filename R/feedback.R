#' Create excel sheets with feedback for countries. Returns an excel workbook
#' for each country with one sheet showing missings and another showing threshold
#' flags, as well as recommendations for each.
#' Requires WHO IDSR data which has been prepared using the {clean_data} function,
#' and then run through the {missing_checker} and {threshold_checker} functions,
#' and all being stored in a list with a data frame for each country.
#'
#' @param x A list of data frames which has been processed with {clean_data},
#'  {missing_checker} and {threshold_checker}.
#' @param current_week The week of interest as a character or {aweek} object.
#' Needs to be in the correct format ("YYYY-Www").
#' The default "2018-W35" is just to demonstrate the necessary format.
#' @param output_path File path where you would like outputs to be saved to.
#' Within this folder a new folder will be created and named after the current week.
#' This defaults to your current directory, with subfolders of data > outputs > verification.
#' These folders need to exist already - only the current week folder will be created.
#'
#' @importFrom dplyr filter select mutate
#' @importFrom aweek as.aweek
#' @importFrom purrr map
#' @importFrom stringr str_glue
#' @importFrom rio export
#' @importFrom here here
#'
#' @seealso \code{\link{clean_data}} for preparing IDSR data for use with threshold_checker,
#' \code{\link{missing_checker}} for missing flags and \code{\link{threshold_checker}}
#' for threshold flags.
#'
#' @export
country_feedback <- function(x,
                             current_week = "2018-W35",
                             output_path  = here::here("Data", "Outputs", "Verification")
                             ) {

  ## make current_week an {aweek} object
  current_week <- aweek::as.aweek(current_week)

  ## filter to messages to the week of interest
  message_list <- map(x, ~filter(.,
                                 epiweek == current_week &
                                   !is.na(miss_comment)))

  ## subset messages to minimal dataset
  message_list <- map(message_list,
                      ~select(.,
                              province,
                              district,
                              "disease/maladie"                 = disease,
                              "week_year/semaine_annee"         = week_year,
                              "week_number/numero dela semaine" = week_number,
                              "comment/commentaires"            = miss_comment))

  ## add a recommendations columns at end
  message_list <- map(message_list, ~mutate(.,
                                            "Recommendation/Recommandation" =
                                              "Verify and amend the report accordingly/ Vérifier et amender le rapport en consequence"))

  ## filter alters to the week of interest
  alert_list <- map(x, ~filter(.,
                               epiweek == current_week &
                                 !is.na(alert)))


  ## subset thresholds to minimal dataset
  alert_list <- map(alert_list, ~select(.,
                                        province,
                                        district,
                                        "disease/maladie"                 = disease,
                                        "week_year/semaine_annee"         = week_year,
                                        "week_number/numero dela semaine" = week_number,
                                        "cases/cas"                       = number_of_cases,
                                        "deaths/deces"                    = number_of_deaths,
                                        "alert threshold/seuil d'alerte"  = Alert_threshold,
                                        "recommendation/recommandation"   = Recommendation))


  ## create a folder for the week
  week_path <- str_glue(output_path, "/", current_week)

  dir.create(week_path)


  ## export messages as excel for each country seperately to "Missings" sheet

  for (i in names(x)) {

    ## pull together path and name for file
    file_path <- str_glue(week_path, "/", i, "_", current_week, ".xlsx")

    ## export message list to file
    export(message_list[[i]], file = file_path, which = "Missings")

    ## export alert list to file
    export(alert_list[[i]], file = file_path, which = "Alerts")

  }
}
