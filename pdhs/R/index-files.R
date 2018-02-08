index_files <- function(path, ...) {
  all_paths <- dir(path=path, full.names=TRUE)
  filtered_paths <- filter_file_names(path=all_paths, ...)
  
}
