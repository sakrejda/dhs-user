
#' Return a closure with a log file set
#'
#' @param log_file where to write log to
#' @return closure, callable in various ways with strings.
#' @export
logger <- function(log_file = tempfile()) { 
  log_file <- log_file
  log_f <- function(...) cat(paste(..., collapse=", "), file = log_file, sep = "\n", append = TRUE)
  return(log_f)
}


#' Retrieve a job with defaults integrated.
#'
#' @param instructions, loaded from .yaml file (list)
#' @param i pull out i'th job
#' @param logger
#' @return job
#' @export
get_job <- function(instructions, i, logger) {
  job <- instructions[['default']] %>% purrr::list_merge(instructions[[1]])
  logger("Job name: ", job[['name']])
  return(job)
}

#' Retrieve the script to run on.
#'
#' @param job
#' @param logger
get_script <- function(job, logger) {
  script_file = paste0(job[['name']], ".R")
  script_path = find_file(job[['source_dir']], script_file)
  if (length(script_path) == 0) 
    logger("Script not found: ", script_file)
  else
    logger("Script path: ", script_path)
  return(script_path)
}

#' Retrieve the implied name of the main function for a script.
#'
#' @param job
#' @param logger
#' @return function name as character vector, length-1
#' @export
get_function <- function(job, logger) {
  log("Finding main function.")
  function_name <- gsub('-', '_', job[['name']])
  function_found <- function_name %in% ls(parent.frame())
  if (!function_found) 
    logger("Function ", function_name, " not found, likely fail.")
  f <- get(function_name, parent.frame())
  return(f)
}

#' Return the expected outputs from a job:
#'
#' @param job a job
#' @param logger, a logger
#' @return character vector of expected object names
#' @export
get_expectations <- function(job, logger) { 
  expect_file <- sapply(job[['outputs']], function(x) x[['file']])
  expect_name <- gsub('\\.[a-zA-Z0-9]+$', '', expect_file)
  expect <- gsub('-', '_', expect_name)
  return(expect)
}


#' Save output
#'
#' @param job job description
#' @param output list of output objects
#' @param logger logger to write log to...
save_output <- function(job, output, logger) {
  target_dir <- job[['target_dir']]
  output_files <- sapply(job[['outputs']], function(x) x[['file']])
  output_names <- get_expectations(job, logger)
  for (i in 1:length(output_names)) {
    output_path <- file.path(target_dir, output_files[i])
    saveRDS(output[[output_names[i]]], output_path)
  }
  return(NULL)
}

#' Run files based on scripts... take N+1...
#' 
#' @param file .yaml file with instructions, see example
#' @param log_file where to write text log to.
#' @return log_file where logs were written.
#' @export
scripted <- function(file, log_file = tempfile(), debug=FALSE) {
  instructions <- yaml::yaml.load_file(file)
  n_instructions <- length(instructions[['runs']])
  log <- logger(log_file)
  log("There are ", n_instructions, " jobs.")

  for (i in 1:n_instructions) {
    log("Instruction ", i)
    job <- get_job(instructions, i, log)

    log(paste("Search for dependencies in: ", job[['source_dir']]))
    script_path = get_script(job, log)

    log("Loading script.", script_path)
    source(script_path, echo = debug)

    log("Calling main function.")
    o <- NULL
    o <- do.call(what = get_function(job, log), args = list(source_dir = job[['source_dir']]))

    if (!is.null(o)) {
      log("Objects in return are: ")
      log(names(o))
    } else {
      log("No objects found in return.")
    }

    expected_objects <- get_expectations(job, log)
    log("Expected objects are: ")
    log(expectted_objects)
    if (all(expected_objects %in% names(o))) {
      log("All output found.")
    } else {
      log("Some expected output is missing.")
      missing_output <- expected_objects[!(expected_objects %in% names(o))]
      for (missing in missing_output) {
        log("Object named ", missing, " was not found.")
      }
    }

    log("Saving output.")
    log("Target directory is: ", job[['target_dir']])
    save_output(job, o, log) 
    log("Finished saving output.")
  }
  return(log_file)
}



