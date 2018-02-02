

#' This example assumes there is a directory called 'source' where you have some
#' DHS .zip files (well, usually .ZIP but it doesn't matter).  It gets paths to
#' all those files and filters based on the format code so that only paths to Stata
#' version of the women's files ("Individual Recode" or "IR") remain.  Finally
#' the script unzips those files picking out only the files with the .dta 
#' extension, which are the Stata files we need (typically even .ZIP files that
#' contain Stat files will have other data).

output_dir <- 'target'  # This is where output will go

# Here we just get all .zip files
source_files <- dir(path = 'source', pattern = '\\.ZIP', full.names=TRUE, ignore.case=TRUE)

# I don't remember what the format code was for Stat so grab that:
format_code <- pdhs::get_file_format_code("Stata")

# Filter using the Stata format code as well as the "IR" dataset code:
files_on_women <- pdhs::filter_file_names(source_files, dataset="IR", format=format_code)

# Unzip only .dta files to the output directory.
pdhs::filtered_unzip(files_on_women, 'dta', output_dir)



