#' For a path return a vector of full paths to contained
#' file names, filtered by codes.
#'
#' @param path path to search for files
#' @param ... arguments passed on to 'filter_file_names'
#' @return paths to file names, limited by filtering operation
index_files <- function(path, ...) {
  all_paths <- dir(path=path, full.names=TRUE)
  filtered_paths <- filter_file_names(path=all_paths, ...)
  return(filtered_paths)  
}

