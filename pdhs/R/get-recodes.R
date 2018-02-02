# libs: xml2, rvest, lubridate

#' Current location of the DHS recode manuals. 
#' @return XML version of the page containing links to
#'         manuals.
get_recode_manual_page <- function() xml2::read_html("https://www.dhsprogram.com/publications/publication-dhsg4-dhs-questionnaires-and-manuals.cfm") 

#' Retrieves the links to the recode manuals for all 
#' rounds.
#' @return url for each recode manual and the associated map .pdf
get_manual_paths <- function() get_recode_manual_page() %>% 
  rvest::html_nodes("body #download_btn a") %>% 
  gsub(pattern='.*href="', replacement="", x=.) %>% 
  gsub(pattern='\\.pdf.*', replacement='.pdf', x=.) %>% 
  file.path("https://www.dhsprogram.com", .)

#' Download recode manuals
download_manuals <- function(output_dir=static()) {
  manual_paths <- get_manual_paths() 
  manual_paths %>%  sapply(function(f) download.file(url=f, destfile=file.path(output_dir, basename(f))))
  return(file.path(output_dir, basename(manual_paths)))
}




