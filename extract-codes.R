

#' For a filename 'file', extract the country code (first two characters).
#' 
#' @param file filename to extract from.
extract_country_code <- function(file) basename(file) %>% substr(start=1, stop=2)

#' For a filename 'file', extract the dataset type code (3rd and 4th characters).
#' 
#' @param file filename to extract from.
extract_dataset_type_code <- function(file) basename(file) %>% substr(start=3, stop=4)

#' For a filename 'file', extract the DHS round code (5th character).
#' 
#' @param file filename to extract from.
extract_dhs_round_code <- function(file) basename(file) %>% substr(start=5, stop=5)

#' For a filename 'file', extract the within-round data release code (6th character).
#' 
#' @param file filename to extract from.
extract_dhs_release_code <- function(file) basename(file) %>% substr(start=6, stop=6)

#' For a filename 'file', extract the data format code (7th and 9th characters).
#' 
#' @param file filename to extract from.
extract_format_code <- function(file) basename(file) %>% substr(start=7, stop=8)



