#' @name clean_fs_dates
#' @title Correct dates in financial statements
#' @author Nicolas Mangin
#' @description Function correcting dates and reporting dates to make them consistent with SEC filings' information
#' @param finstat Tibble. Statements gathered from csv files.
#' @param filings Tibble. List of SEC filings.
#' @return Tibble. Financial statements with clean dates
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr sample_n
#' @importFrom dplyr select
#' @importFrom dplyr slice_max
#' @importFrom dplyr ungroup
#' @importFrom lubridate ymd
#' @export


clean_fs_dates <- function(finstat, filings){
  
  cik <- NULL
  reportDate <- NULL
  newDate <- NULL
  gap <- NULL
  statement <- NULL
  
  
  filingDates <- filings |>
    dplyr::select(cik, newDate = reportDate) |>
    stats::na.omit() |>
    base::unique() |>
    dplyr::mutate(newDate = lubridate::ymd(newDate))
  
  newReportDates <- finstat |>
    dplyr::select(cik, reportDate) |>
    stats::na.omit() |>
    base::unique() |>
    dplyr::left_join(filingDates, by = "cik", relationship = "many-to-many") |>
    dplyr::mutate(
      filingDate = dplyr::case_when(
        base::is.na(filingDate) ~ reportDate,
        TRUE ~ filingDate
      ),
      acceptedDate =  dplyr::case_when(
        base::is.na(acceptedDate) ~ reportDate,
        TRUE ~ acceptedDate
      ),
      newDate =  dplyr::case_when(
        base::is.na(newDate) ~ reportDate,
        TRUE ~ newDate
      )
    ) |>
    dplyr::mutate(gap = reportDate - newDate) |>
    dplyr::group_by(cik, reportDate) |>
    dplyr::filter(gap < 10, gap == base::min(base::abs(gap))) |>
    dplyr::sample_n(1) |>
    dplyr::ungroup() |>
    dplyr::select(-gap)
  
  newDates <- finstat |>
    dplyr::select(cik, date) |>
    stats::na.omit() |>
    base::unique() |>
    dplyr::left_join(filingDates, by = "cik", relationship = "many-to-many") |>
    dplyr::mutate(gap = date - newDate) |>
    dplyr::group_by(cik, date) |>
    dplyr::filter(gap < 10, gap == base::min(base::abs(gap))) |>
    dplyr::sample_n(1) |>
    dplyr::ungroup() |>
    dplyr::select(-gap)
  
  finstat |>
    dplyr::left_join(newReportDates, by = c("cik","reportDate")) |>
    dplyr::mutate(reportDate = dplyr::case_when(
      !base::is.na(newDate) ~ newDate,
      TRUE ~ reportDate
    )) |>
    dplyr::select(-newDate) |>
    dplyr::left_join(newDates, by = c("cik","date")) |>
    dplyr::mutate(date = dplyr::case_when(
      !base::is.na(newDate) ~ newDate,
      TRUE ~ date
    )) |>
    dplyr::select(-newDate) |>
    dplyr::group_by(cik, date, statement) |>
    dplyr::slice_max(reportDate) |>
    dplyr::ungroup() |>
    dplyr::select(-reportDate)
}
