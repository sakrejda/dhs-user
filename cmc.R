# libs: xml2, rvest, lubridate


#' Convert century-month-code to date
#'
#' @param cmc, the number of months since the beginning of the 20th century.
#'        January 1900 is CMC 1.
cmc_to_date <- function(cmc) lubridate::ymd("1900-01-01") + lubridate::months(cmc - 1)

#' Convert century-month-code to a year
#' @param cmc, the number of months since the beginning of the 20th century.
#'        January 1900 is CMC 1.
cmc_to_year <- function(cmc) ((cmc - 1)/12) %>% as.integer + 1900


#' Convert century-month-code to a month
#' @param cmc, the number of months since the beginning of the 20th century.
#'        January 1900 is CMC 1.
cmc_to_month <- function(cmc) cmc - (cmc_to_year(cmc) - 1900) * 12



