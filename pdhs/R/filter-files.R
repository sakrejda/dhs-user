
#' Check if a string has a specific extension, fails without 
#' extension, returns TRUE on NULL extension.  This function
#' is dumb and when it encounters filenames with multiple dots
#' it only considers the final extension.
#'
#' @param s string to check for the extension
#' @param e string specifying the extension.  
#' @export
has_extension <- function(s, e) ifelse(is.null(e), 
  rep(TRUE, length(s)), 
  grepl(pattern=paste0('\\.', e, '$'), x=s, ignore.case=TRUE))

#' Check if a string has the .dta extension.
#' @param s string to check
#' @export 
is_dta <- function(s) has_extension(s, 'dta')

#' Filter a path for DHS data file name elements.  Returns 
#' true for path items that match the requested elements and
#' false for those that don't.  The match considers the 
#' position of the element in the filename so it's not a straight
#' regex.
#'
#' To avoid situations with unreliable filtering (e.g.-try typing
#' "Lao People's Democratic Republic" reliably/repeatedly) this
#' function relies on the DHS codes for countries, data formats, 
#' etc... to get these use the functions 'get_country_code', 
#' 'get_dataset_type_code', 'get_file_format_extensions', 
#' 'get_file_format_code'.  Those functions to loose string matching
#' via regex.
#'
#' @param path single path or vector of paths to filter against.
#' @param country country CODE (not name) to filter with.
#' @param dataset dataset type CODE to filter with.
#' @param round round number (e.g.-3 for DHS-III).
#' @param release release version code (not always numeric, varies 
#'        by round and country). This function should really support
#'        'latest' as a special value.
#' @param format file format CODE (not extension or name) to filter
#'        with.
#' @export
filter_file_name <- function(path, country=TRUE, dataset=TRUE, round=TRUE, release=TRUE, format=TRUE) {
  if (!isTRUE(country))
    country <- country == extract_country_code(path)
  if (!isTRUE(dataset))
    dataset <- dataset == extract_dataset_type_code(path)
  if (!isTRUE(round))
    round <- round == extract_dhs_round_code(path)
  if (!isTRUE(release))
    release <- release == extract_dhs_release_code(path)
  if (!isTRUE(format))
    format <- format == extract_format_code(path)
  return(country & dataset & round & release & format)
}

#' Takes a path or vector of paths and filters either using
#' a regex pattern or based on DHS codes.  For full details
#' of code-based filtering see ?filter_file_name 
#'
#' @param path path or vector of paths to files to filter
#'        out.  
#' @param either NULL (for code based filtering) or a regex
#'        to be used to filter out elements of the path.
#' @return paths matching the conditions or a length-zero
#'         character vector.
#' @export
filter_file_names <- function(path, pattern=NULL, ...) {
  if (!is.null(pattern)) 
    return(path[grepl(pattern=pattern, x=path)])
  o <- vector(mode='character', length=0)
  for (p in path) {
    if (filter_file_name(p, ...))
      o <- c(o, p)
  }
  return(o)
}



