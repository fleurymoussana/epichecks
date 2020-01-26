#' Set the class of IDSR processed data to be able to merge countries in to one
#' dataset.
#'
#' @param x A data frame
#' @param output_file Path to where you would like to save a RDS file of merge data
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr map modify_at
#' @importFrom here here
#'
#' @seealso \code{\link{clean_data}} for preparing IDSR data for use with threshold_checker,
#' \code{\link{missing_checker}} for missing flags and \code{\link{threshold_checker}}
#' for threshold flags.
#'
#' @export
aggregator <- function(x,
                       output_file = here::here("Data", "Outputs", "Merged.Rds")) {
  ## apply the function to each dataset in list
  x <- purrr::map(x, change_class)

  ## aggregate data in to one big dataset
  ## bind all dataframes together as rows
  x <- bind_rows(x)

  ## Save aggregated dataset in Rdataset file format
  saveRDS(x, file = output_file)

  ## return dataset
  x
}




change_class <- function(y) {
  ## set numeric variables
  y <- modify_at(y,
                 c("week_year",
                   "week_number",
                   "population",
                   "number_of_cases",
                   "number_of_deaths",
                   "acquisition_source_id"),
                 as.numeric)

  ## set character variables
  y <- modify_at(y,
                 c("disease",
                   "country",
                   "province",
                   "district",
                   "age_range"),
                 as.character)

  ## return the edited dataframe
  y
}
