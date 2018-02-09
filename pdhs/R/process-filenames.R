
#' Extract country name from file name.
#' 
#' @param file name to extract from.
#' @return return the long-form country name used by DHS. 
#' @export
extract_country <- function(file) {
  code <- extract_country_code(file) 
  map <- get_country_code_map() 
  return(ifelse(code %in% names(map), map[code], NA))
}

#' Extract the dataset type from the file name.
#' 
#' @param file name to extract from.
#' @return return the long-form of the datatype used by DHS.
#' @export
extract_dataset_type <- function(file) {
  code <- extract_dataset_type_code(file)
  map <- get_dataset_type_map() 
  return(ifelse(code %in% names(map), map[code], NA))
}  

#' Extract the DHS round from the file name.
#' 
#' @param file name to extract from.
#' @return return a digit for the DHS round (e.g.-DHS-III -> 3)
#' @export
extract_dhs_round <- function(file) extract_dhs_round_code(file)

#' Extract the DHS round's data release version from the file name.
#' 
#' @param file name to extract from.
#' @return return an alphanumeric from the DHS file name. Usually a digit.
#' @export
extract_dhs_release <- function(file) extract_dhs_release_code(file) 

#' Extract the file format from the file name.
#' 
#' @param file name to extract from.
#' @return return the long-form of the file format
#' @export
extract_format <- function(file) {
  code <- extract_format_code(file) 
  map <- get_file_format_map()
  return(ifelse(code %in% names(map), map[code], NA))
}

#' Extract all filename components into meaningful strings
#' using the more specific functions.  
#' 
#' @param path path to file to extract from.
#' @return a named vector with the translated long-form
#'         file name components.
#' @export
process_filenames <- function(path) data.frame(
    path=path, 
    stub = extract_survey_stub(path) %>% as.character,
    country = extract_country_code(path) %>% as.character, 
    dataset_type = extract_dataset_type_code(path) %>% as.character,
    round = extract_dhs_round_code(path) %>% as.character, 
    release = extract_dhs_release_code(path) %>% as.character,
    format = extract_format_code(path) %>% as.character, 
    stringsAsFactors=FALSE)

#' Specialize for just one path element, error otherwise.
#' @param path path to file to extract from.
#' @return a named vector with the translated long-form
#'         file name components.
#' @export
process_filename <- function(path) {
  if (isTRUE(length(path) != 1)) {
    stop("Multiple paths so you want 'process_filenames'")
  } else {
    return(process_filenames(path)[1,])
  } 
} 
