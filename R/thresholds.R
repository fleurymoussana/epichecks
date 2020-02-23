#' Flag weeks with case or death counts which are higher than the threshold defined
#' in threshold dictionary.
#' Requires WHO IDSR data which has been prepared using the {clean_idsr} function.
#' Will return variables for unique regions, alerts, and recommendations.
#'
#' @param x A data frame
#'
#' @importFrom dplyr arrange mutate group_by case_when if_else ungroup
#' @importFrom stringr str_glue
#'
#' @seealso \code{\link{clean_idsr}} for preparing IDSR data for use with threshold_checker
#'
#' @export
threshold_checker <- function(x) {

  ## sort dataset by year, week, disaease and area
  x <- arrange(x,
               epiweek, disease, country, province, district)

  ## create unique region identifiers
  x <- mutate(x, unique_regions = str_glue("{province}_{district}"))

  ## group by area
  x <- group_by(x, unique_regions, disease)


  x <- mutate(x, alert = case_when(

    ## for case count
    Threshold_type == "Case" & number_of_cases >= 1 ~   "Flag",

    ## for death counts
    Threshold_type == "Death" & number_of_deaths >= 1 ~ "Flag",

    ## for meningitis based on straight counts
    ## (population not always available)
    Threshold_type == "Meningitis" &
      number_of_cases >= 2 ~                            "Flag",

    ## for measles based on straight counts
    Threshold_type == "MMR" & number_of_cases >= 5 ~    "Flag",

    ## for relative counts
    Threshold_type == "Relative" &
      number_of_cases > lag(number_of_cases) ~          "Flag",


    ## for malaria based on multiple of previous week
    Threshold_type == "Malaria" &
      number_of_cases >= lag(number_of_cases) * 1.5 ~   "Flag",

    ## else missing
    TRUE ~ NA_character_
  )
  )

  ## change the recommendation to NA where alert is not flagged
  x <- mutate(x, Recommendation = if_else(alert != "Flag",
                                          NA_character_,
                                          Recommendation))

  ## ungroup and return dataset
  ungroup(x)


}
