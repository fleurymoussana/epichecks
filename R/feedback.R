#' Create excel sheets with feedback for countries. Returns an excel workbook
#' for each country with one sheet showing missings and another showing threshold
#' flags, as well as recommendations for each.
#' Requires WHO IDSR data which has been prepared using the {clean_idsr} function,
#' and then run through the {missing_checker} and {threshold_checker} functions,
#' and all being stored in a list with a data frame for each country.
#'
#' @param x A list of data frames which has been processed with {clean_idsr},
#'  {missing_checker} and {threshold_checker}.
#' @param current_year The year of interest as numeric (e.g. the default is 2020)
#' @param current_week The week of interest as numeric. The default is 1.
#' Values less than 10 are padded with leading zeros, e.g. 1 becomes 01. (You
#' could also enter 01 and it would be fine)
#' @param output_path File path where you would like outputs to be saved to.
#' Within this folder a new folder will be created and named after the current week.
#' This defaults to your current directory, with subfolders of data > outputs > verification.
#' The year folder needs to exist already - only the current week folder will be created.
#'
#' @importFrom dplyr filter select mutate
#' @importFrom aweek as.aweek
#' @importFrom purrr map
#' @importFrom stringr str_pad str_glue
#' @importFrom rio export
#' @importFrom here here
#'
#' @seealso \code{\link{clean_idsr}} for preparing IDSR data for use with threshold_checker,
#' \code{\link{missing_checker}} for missing flags and \code{\link{threshold_checker}}
#' for threshold flags.
#'
#' @export
country_feedback <- function(x,
                             current_year = 2020,
                             current_week = 1,
                             output_path  = here::here("Data Files", "Output")
                             ) {

  ## pad the current week so it is always two values long
  current_week <- str_pad({current_week}, 2, pad = 0)

  ## set the current epiweek
  current_epiweek <- aweek::as.aweek(str_glue("{current_year}-W{current_week}"))

  ## filter to messages to the week of interest
  message_list <- map(x, ~filter(.,
                                 epiweek == current_epiweek &
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
                               epiweek == current_epiweek &
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


  ## pull together file path for the current week
  week_path <-  str_glue(output_path, "/",
                         current_year, "/",
                         current_week)

  ## create a folder for the week
  dir.create(week_path)


  ## export messages as excel for each country seperately to "Missings" sheet

  for (i in names(x)) {

    ## pull together path and name for file
    file_path <- str_glue(week_path, "/", i, "_", current_epiweek, ".xlsx")

    ## export message list to file
    export(message_list[[i]], file = file_path, which = "Missings")

    ## export alert list to file
    export(alert_list[[i]], file = file_path, which = "Alerts")

  }

  ## return data frames of message list and alert list
  list(message_list = message_list,
       alert_list = alert_list)
}
