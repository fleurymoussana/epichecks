#' Create an excel summary report of data submitted for week by each country.
#'
#' @param x A data frame of IDSR data which has been created from {aggregator}
#' @param current_year The year of interest as numeric (e.g. the default is 2020)
#' @param current_week The week of interest as numeric. The default is 1.
#' Values less than 10 are padded with leading zeros, e.g. 1 becomes 01. (You
#' could also enter 01 and it would be fine)
#' @param output_path File path where you would like outputs to be saved to.
#' Within this folder a new folder will be created and named after the current week.
#' This defaults to your current directory, with subfolders of data > outputs > verification.
#' The year folder needs to exist already - only the current week folder will be created.
#'
#' @importFrom dplyr mutate filter group_by summarise if_else tibble left_join select n
#' @importFrom aweek week2date
#' @importFrom stringr str_sub str_glue
#' @importFrom tidyr replace_na
#' @importFrom here here
#' @importFrom rio export
#'
#' @seealso \code{\link{clean_idsr}} for preparing IDSR data for use with threshold_checker,
#' \code{\link{missing_checker}} for missing flags and \code{\link{threshold_checker}}
#' for threshold flags.
#'
#' @export
weekly_summary <- function(x,
                           current_year = 2020,
                           current_week = 1,
                           output_path  = here::here("Data Files", "Output")
                           ) {


    ## define AFRO countries ,
    afro_countries <- c("Algeria", "Angola",
                        "Benin", "Botswana", "Burkina Faso", "Burundi",
                        "Cameroon", "Cape Verde", "Central African Republic",
                        "Chad", "Comoros", "Congo", "Côte d'Ivoire",
                        "Democratic Republic of the Congo",
                        "Equatorial Guinea", "Eritrea", "eSwatini", "Ethiopia",
                        "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                        "Kenya",
                        "Lesotho", "Liberia",
                        "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius",
                        "Mozambique",
                        "Namibia", "Niger", "Nigeria",
                        "Rwanda",
                        "Sao Tome and Principe", "Senegal", "Seychelles",
                        "Sierra Leone", "South Africa", "South Sudan",
                        "Tanzania (Mainland)", "Tanzania (Zanzibar)", "Togo",
                        "Uganda",
                        "Zambia", "Zimbabwe")


    ## pad the current week so it is always two values long
    current_week <- str_pad({current_week}, 2, pad = 0)

    ## set the current epiweek
    current_epiweek <- aweek::as.aweek(str_glue("{current_year}-W{current_week}"))

    ## set reporting deadline as the wednesday after week of interest (i.e. in week after)
    ## function returns monday of current_week, add 9 days to that gives followed Wed
    report_deadline <- week2date(current_epiweek) + 9


    ## fix the date submitted var
    ## change to date (the original input is day/month/year)
    x <- mutate(x,
                DataSubmissionDate = as.Date(DataSubmissionDate,
                                             origin = "1899-12-30")
                )


    ## get numbers for weekly summary report for each country

    ## filter for current week of interest
    summary_counts <- filter(x, epiweek == current_epiweek)

    ## do for each country individually
    summary_counts <- group_by(summary_counts, country)

    ## create summary table
    summary_counts <- summarise(summary_counts,
        nb_dis        = length(unique(disease)),      # count diseases reported
        nb_reg        = length(unique(district)),     # count districts reported
        week_report   = epiweek[1],                   # pull week number
        submitted     = "Yes",                        # all datasets present therefor Yes

        ## if the date of submission is greater than report deadline, not on time
        on_time       = if_else(max(DataSubmissionDate) > report_deadline,
                                "No", "Yes"),

        ## get a count variable for number of missing reports
        dis_reps      = sum(!is.na(miss_comment)),

        ## get count of total number reports
        dis_reps_tot  = n(),

        ## number of missing messages divided by number of reports times hundred - then categorised
        missings      = round(sum(!is.na(miss_comment)) / n() * 100,
                              digits = 1),

        reg_threshold = length(unique(district[!is.na(alert)])),
        dis_threshold = length(unique(disease[!is.na(alert)]))
      )


    ## create an empty dataframe of all afro countries
    summary_report <- tibble(Country = afro_countries)

    ## join the counts of existing countries to the empty list
    summary_report <- left_join(summary_report, summary_counts,
                                by = c("Country" = "country"))

    ## fill in the zeros for not reporting
    summary_report <- mutate(summary_report,
                             nb_dis      = replace_na(nb_dis, 0),
                             nb_reg      = replace_na(nb_reg, 0),
                             week_report = replace_na(week_report, current_epiweek),
                             submitted   = replace_na(submitted, "No"),
                             on_time     = replace_na(on_time, "Not applicable")
                             )

    summary_report <- select(summary_report,
              Country,
             "Nb of diseases/conditions under surveillance" = nb_dis,
             "Nb districts" = nb_reg,
             week_report,
             "Report submitted" = submitted,
             "Report submitted on time" = on_time,
             "Nb disease reports missing case OR death counts" = dis_reps,
             "Total Nb disease reports" = dis_reps_tot,
             "% missing data" = missings,
             "Nb of districts with surprassed threshold (request for verification)" = reg_threshold,
             "Nb of diseases/conditions with surpassed threshold (request for verification)" = dis_threshold)


    ## pull together file path for the current week
    week_path <-  str_glue(output_path, "/",
                           current_year, "/",
                           current_week)

    ## save summary in appropriate week folder as excel
    ## pull together path and name for file
    file_path <- str_glue(week_path, "/SummaryReport_", current_epiweek, ".xlsx")

    ## export message list to file
    export(summary_report, file = file_path, which = current_epiweek)

    ## return dataset
    summary_report
}
