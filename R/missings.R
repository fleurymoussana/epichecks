#' Flag weeks with missing counts for cases or deaths.
#' Requires WHO IDSR data which has been prepared using the {clean_idsr} function.
#' Will return variables for missing cases, missing deaths and a combined variable
#' with both in.
#'
#' @param x A data frame
#'
#' @importFrom dplyr filter mutate if_else mutate_at na_if
#' @importFrom stringr str_glue
#'
#' @seealso \code{\link{clean_idsr}} for preparing IDSR data for use with missing_checker.
#'
#' @export
missing_checker <- function(x) {

  ## filter dataset for those missing cases or deaths
  x <- filter(x, (is.na(number_of_cases) | is.na(number_of_deaths)))

  ## create seperate variables with comments if missing case or deaths
  x <- mutate(x, case_miss = if_else(is.na(number_of_cases) &
                                       (Threshold_type != "Death" |
                                          is.na(Threshold_type)),
                                     "Cases count missing",
                                     ""))
  x <- mutate(x, death_miss = if_else(is.na(number_of_deaths),
                                      "Deaths count missing",
                                      ""))

  ## create one variable that combines the two comments in one
  x <- mutate(x, miss_comment = str_glue("{case_miss}; {death_miss}"))

  ## switch missings to NA for case_miss and death_miss vars
  x <- mutate_at(x,
                 .vars = c("case_miss", "death_miss"),
                 .funs = ~na_if(., "")
  )

  ## switch missings to NA for comments var
  x <- mutate(x, miss_comment = na_if(miss_comment, "; " ))


  ## return edited dataset
  x
}
