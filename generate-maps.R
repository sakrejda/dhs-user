
#' This downloads the page for the URL that _currently_ has the 
#' DHS country codes... could probably look up what standard they
#' are using but IDK right now.
#' @return an XML of the web page containing this info.
get_country_code_page <- function() xml2::read_html("https://dhsprogram.com/data/File-Types-and-Names.cfm")

#' Return a hacked-together named vector connecting country
#' DHS codes to country names.
#' @return a vector with codes for names and country names for values.
get_country_code_map <- function() {
  session_tempdir <- tempdir()
  map_file <- file.path(session_tempdir, 'country-code-map.rds')
  if (file.exists(map_file)) {
    coding <- readRDS(map_file)
    return(coding)
  } else {
    raw_table <- get_country_code_page() %>% 
      rvest::html_node("body #CS_Element_countrycodes")  %>% 
      rvest::html_node("table") %>% rvest::html_table(header=TRUE)
    coding <- c(raw_table[[2]][2:(length(raw_table[[2]])-1)], raw_table[[4]][2:length(raw_table[[4]])])
    names(coding) <- c(raw_table[[1]][2:(length(raw_table[[1]])-1)], raw_table[[3]][2:length(raw_table[[3]])])
    saveRDS(coding, map_file)
  }
  return(coding)
}

#' This downloads the page for the URL that _currently_ has the 
#' DHS dataset type codes.
#' @return an XML of the web page containing this info.
get_dataset_type_page <- function() get_country_code_page()

#' Return a hackeed-together named vector connecting country 
#' DHS dataset types to their descriptions.
#' @return a vector with codes for names and descriptions for values.
get_dataset_type_map <- function() {
  session_tempdir <- tempdir()
  map_file <- file.path(session_tempdir, 'dataset-type-map.rds')
  if (file.exists(map_file)) {
    coding <- readRDS(map_file)
    return(coding)
  } else {
    raw_table <- get_dataset_type_page() %>%
      rvest::html_node("body #CS_Element_datatypefiles")  %>% 
      rvest::html_node("table") %>% rvest::html_table(header=TRUE)
    coding <- raw_table[[2]]
    names(coding) <- raw_table[[1]]
    saveRDS(coding, map_file)
  }
  return(coding)
}

#' Return a named vector describing data formats and
#' their DHS codes.
#' @return named vector with data format codes for names and
#'         format names for values.
get_file_format_map <- function() c(
  FL = "flat data file", SV = "SPSS data file",
  DT = "Stata data file", SD = "SAS data file")

#' Return a named vector describing data format extensions and
#' their DHS codes.
#' @return named vector with data format codes for names and
#'         format extensions for values.
get_file_format_extension_map <- function() list(
  SV = c("SPS", "SAV"),
  DT = c("DCT", "DO", "DTA"),
  SD = c("SAS7BDAT", "SAS")
)




