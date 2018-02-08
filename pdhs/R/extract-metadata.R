
#' Extract column labels to match column codes.
#'
#' @param data data frame with embedded column
#'        labels in 'label' attribute.
#' @return named vector with names being codes and
#'         values being labels.
extract_column_labels <- function(data) sapply(data, attr, 'label')


#' Extract column value labels to match column value codes.
#'
#' @param data data frame with embedded column
#'        value labels in 'labels' attribute.
#' @return named vector with names being codes and
#'         values being labels.
extract_value_labels <- function(data) {
  l <- sapply(data, attr, 'labels')
  o <- lapply(l, function(x) {
    y <- names(x) 
    names(y) <- x
    return(y)
  })
  return(o)
}

#' Get column label matching a column code
#' @param c code to search for
#' @param data data frame with embeded column labels
get_column_label <- function(c, data) extract_column_labels(data)[c]
   
#' Get column code matching a search string
#' @param s string to search for
#' @param data data frame with embeded column labels
#' @param ... additional arguments to regex search.
get_column_code <- function(s, data, ...) {
  map <- extract_column_labels(data)
  o <- search_reverse_map(s, map, ...)
  return(o)
}

#' Get value label matching a value code
#' @param c column code to use
#' @param data data frame with embeded column value labels.
#' @param 
get_value_labels <- function(c, data, column) extract_value_labels(data)[[column]][as.character(c)]

#' Get value code matching a search string
#' @param s string to search for
#' @param data data frame with embeded column labels
#' @param ... additional arguments to regex search.
get_value_code <- function(s, data, column, ...) {
  l <- extract_value_labels(data)[[column]]
  o <- search_reverse_map(s, map, ...)
  return(o)
}
