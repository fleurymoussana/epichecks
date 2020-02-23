#' Clean WHO IDSR processed data for use in weekly / monthly reports.
#' Renaming of diseases based on disease dictionary, setting threshold
#' type for disease based on threshold dictionary and creating an epiweek
#' variable.
#' Will add variables to your data set including: epiweek, threshold_type,
#' Alert_threshold and Recommendation (note the last three are placeholders to be
#' used for threshold_checker)
#'
#' @param x A data frame
#'
#' @importFrom rio import
#' @importFrom dplyr mutate mutate_at select if_else left_join
#' @importFrom matchmaker match_vec
#' @importFrom stringr str_pad str_glue
#' @importFrom aweek as.aweek
#' @export
clean_idsr <- function(x) {

  ## throw warning if week numbers above 60
  if (max(x$week_number, na.rm = TRUE) >= 60) {
    warning("Week number over 60 reported. Will be ignored and epiweek will be
    set to NA for these rows")
  }


  ## get excel file path for disease dictionary
  disease_path <- system.file("extdata", "disease_dictionary.xlsx", package = "epichecks")

  ## read in the disease dictionary from internal package folder
  dis_dict <- import(disease_path)

  ## get excel file path for threshold dictionary
  threshold_path <- system.file("extdata", "threshold_dictionary.xlsx", package = "epichecks")

  ## read in the threshold dictionary from internal package folder
  threshold_dict <- import(threshold_path)

  ## create a new variable with the old disease names
  x <- mutate(x, original_categories = disease)

  ## recode disease names based on dictionary
  x$disease <- matchmaker::match_vec(x$disease,
                                     dictionary = dis_dict,
                                     from = "disease_old",
                                     to = "disease_new")

  ## change encoding in geographical names to accept special characters
  x <- mutate_at(x,
                 .vars = c("country", "province", "district"),
                 .funs = ~iconv(., from = "LATIN1", to = "UTF-8")
  )


  ## creates an epiweek variable
  x <- mutate(x,
              ## add leading zeros to weeks with single umber
              weekpad = str_pad({week_number}, 2, pad = 0),
              ## glue together
              weekpad2 = if_else(week_number < 60,
                                 str_glue("{week_year}-W{weekpad}-1"),
                                 NA_character_)
  )

  ## create an epiweek variable
  x <- mutate(x,
              epiweek = aweek::as.aweek(weekpad2, floor_day = TRUE))

  ## drop weekpad
  x <- select(x, -weekpad, -weekpad2)

  ## create new var with whether or not disease defined in threshold dictionary
  x <- left_join(x,
                 select(threshold_dict,
                        Disease, Threshold_type, Alert_threshold, Recommendation),
                 by = c("disease" = "Disease"))

  ## return dataset
  x
}
