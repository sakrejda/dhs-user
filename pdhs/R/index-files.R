#' Find a full path to a file name, recursively
#'
#' @param root search everything below this directory
#' @param name file name to search for
#' @return full normalized path to file
find_file <- function(root, name, complete = TRUE) {
  if (fixed) 
    name = paste0('^', name, '$')
  if (length(name) > 1) 
    name = paste0(name, collapse = '|')
  path = dir(path = root, pattern = name, recursive=TRUE, full.names=TRUE)
  normalized_path = normalizePath(path)
  return(normalized_path)
}


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

