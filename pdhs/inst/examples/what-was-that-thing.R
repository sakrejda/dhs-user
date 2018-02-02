library(pdhs)

#' Say you have a file-name, e.g., 
file_name <- "LBCR51DT.ZIP"

#' Say you don't know what country it refers to:
extract_country(file_name)

#' Or maybe you're not sure which dataset type "CR" is:
extract_dataset_type(file_name)

#' The round stuff is pretty obvious but you don't
#' want to count characters to use substr:
extract_dhs_round(file_name)
extract_dhs_release(file_name)

#' Or which data file type 'DT' is:
extract_format(file_name)

#' There's also the all-in-one:
process_filename(file_name)


#' And the unexported code-only no-counting version
#' these can be handy for filtering:
pdhs:::extract_country_code(file_name)
pdhs:::extract_dataset_type_code(file_name)
pdhs:::extract_dhs_round_code(file_name)
pdhs:::extract_dhs_release_code(file_name)
pdhs:::extract_format_code(file_name)

#' There are also matching functions that go the other way:
get_file_format_code('SAS', ignore.case=TRUE)
get_dataset_type_code('Men', ignore.case=TRUE)
get_country_code("people", ignore.case=TRUE)





