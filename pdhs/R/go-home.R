#' Returns the package's install directory (for the loaded
#' version).
home <- function() system.file(package="pdhs")

#' Returns the package's scripts directory (for the loaded
#' version). 
scripts <- function() system.file('scripts', package='pdhs')

#' Returns the package's static resource directory (for the 
#' loaded version).
static <- function() system.file('static', package='pdhs')


