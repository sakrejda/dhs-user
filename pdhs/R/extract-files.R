
#' Unzip from a path to a single file or multiple files to a specified output
#' output directory but only if the internal files has a given extension.
#' 
#' @param path path to a file or vector of files to paths to unzip from.
#' @param extension extension to filter for, no filtering if left unspecified.
#' @param output_dir directory to write unzipped files to.
#' @export
filtered_unzip <- function(path, extension=NULL, output_dir=getwd()) {
  if (is.null(path)) stop("Must provide a source zip file or files.")
  if (length(path) != 0 && length(path) > 1) {
    return(sapply(path, filtered_unzip, extension=extension, output_dir=output_dir))
  }
  file_name <- basename(path)
  contained_file_names <- unzip(zipfile=path, list=TRUE)$Name 
  is_format <- has_extension(contained_file_names, extension)
  matching_files <- contained_file_names[is_format]
  if (any(is_format)) {
    unzip(zipfile=path, files=matching_files, overwrite=TRUE, exdir=output_dir)
  }
  return(matching_files)
}


