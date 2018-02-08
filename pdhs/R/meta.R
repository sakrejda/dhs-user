
search_reverse_map <- function(s, map, ...) {
  options <- names(map)
  names(options) <- map
  matches <- grepl(pattern=s, x=map, ...)
  return(options[matches])
}


