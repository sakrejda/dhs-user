extract_country <- function(file) {
  code <- extract_country_code(file) 
  map <- get_country_code_map()  ## FIXME: Shouldn't require web call but does right now.
  return(ifelse(code %in% names(map), map[code], NA))
}

extract_dataset_type <- function(file) {
  code <- extract_dataset_type_code(file)
  map <- get_dataset_type_map()  ## FIXME: Shouldn't require web call but does right now.
  return(ifelse(code %in% names(map), map[code], NA))
}  

extract_dhs_round <- function(file) extract_dhs_round_code(file)
extract_dhs_release <- function(file) extract_dhs_release_code(file) 

extract_format <- function(file) {
  code <- extract_format_code(file) 
  map <- get_file_format_map()
  return(ifelse(code %in% names(map), map[code], NA))
}

process_filename <- function(file) {
  o = c(basename(file), extract_country(file),
    extract_dataset_type(file), extract_dhs_round(file),
    extract_dhs_release(file), extract_format(file),
    use.names=FALSE)
  names(o) <- c('file_name', 'country', 'type',
    'round', 'release', 'format')
  return(o)
}





