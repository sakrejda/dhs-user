
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


#' Allow NULL to evaluate as TRUE for filtering to play
#' nice with missing(...)
#' 
#' @param x either NULL or a vector.  
#' @return either NULL -> TRUE or non-NULL -> self
null_or_compare <- function(x, y) if (is.null(x)) return(TRUE) else return(x == y)

#' Takes a path or vector of paths and filters either using
#' a regex pattern or based on DHS codes.  For full details
#' of code-based filtering see ?filter_file_name 
#'
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
#' @param path path or vector of paths to files to filter
#'        out.  
#' @param either NULL (for code based filtering) or a regex
#'        to be used to filter out elements of the path.
#' @return paths matching the conditions or a length-zero
#'         character vector.
#' @export
filter_file_names <- function(path, pattern=NULL, latest=FALSE, ...) {
  have_dots <- !missing(...)
  if (have_dots) {
    dots <- list(...)
  }

  if (!is.null(pattern)) 
    path = path[grepl(pattern=pattern, x=path)]
  if (!have_dots && !latest) {
    return(path)
  }
  file_data <- process_filenames(path) 
  if (!have_dots && isTRUE(latest)) {
    file_data <- file_data %>%
      dplyr::group_by(country, dataset_type, round, format) %>% 
      dplyr::filter(release == max(release))
    return(file_data[['path']])
  }
  if (isTRUE(latest)) {
    file_data <- file_data %>% 
      dplyr::group_by(country, dataset_type, round, format) %>% 
      dplyr::filter(
        null_or_compare(dots[['country']], country), 
        null_or_compare(dots[['dataset_type']], dataset_type), 
        null_or_compare(dots[['round']], round), 
        release == max(release),
        null_or_compare(dots[['format']], format)) %>%
      dplyr::summarise(path=last(path)) 
    return(file_data[['path']])
  }
  if (!isTRUE(latest)) { 
    file_data <- file_data %>% 
      dplyr::filter(
        null_or_compare(dots[['country']], country), 
        null_or_compare(dots[['dataset_type']], dataset_type), 
        null_or_compare(dots[['round']], round), 
        null_or_compare(dots[['release']], release), 
        null_or_compare(dots[['format']], format))
    return(file_data[['path']])
  }
  stop("Something is rotten in Denmark.")
}



