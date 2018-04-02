self_label <- function(column) {
  label_values <- attr(column, 'labels')
  if (is.null(label_values)) { 
    l_column = column
    attr(column, 'labels') <- NULL
  } else {
    labels <- names(label_values)
    l_column = vector(mode='character', length=length(column))
    for (i in seq_along(label_values)) {
      l_column[column == label_values[i]] <- labels[i] 
    }
  }
  return(l_column) 
}

is_not_using <- function(method) {
  lc = self_label(method)
  is_nu <- grepl(pattern = '^not using', x = lc)
  return(is_nu)
}

get_country <- function(cp) substr(cp, 1, 2)
get_phase <- function(cp) {
  if (all(nchar(cp) == 3)) {
    return(substr(cp, 3, 3))
  } else if (all(nchar(cp) == 2)) {
    return(1)
  }
  stop("No phase in country + phase label.")
}


kenya_phase_year_map = c(`4` = 2003, `5` = 2008, `6` = 2014)

# helper:
# sapply(surveys, function(x) (attr(x$v312, 'labels'))) %>% 
# `names<-`(NULL) %>% do.call(what=c) %>% 
# (function(x) unique(names(x)))() %>% sort %>% dput

is_modern <- function(column) {
  modern_methods <- c("abstinence", "condom", "diaphragm", 
    "female condom", "female sterilization", 
    "foam or jelly", "implants/norplant", "injections", 
    "iud", "lactational amenorrhea", "lactational amenorrhea (lam)", 
    "male sterilization", "norplant", "oher modern method", 
    "pill")
  modern_methods_regex = paste(modern_methods, sep="", collapse="|")
  is_mm <- grepl(pattern = modern_methods_regex, x = column)
  return(is_mm)
}

is_ever_married <- function(column) {
  ever_married <- c("currently in union/living with a man",
    "currently married", "formerly in union/living with a man",
    "formerly married")
  ever_married_regex = paste(ever_married, sep="", collapse="|")
  is_em <- grepl(pattern = ever_married_regex, x = column)
  return(is_em)
}

libs <- c('dplyr', 'ggplot2', 'magrittr')
lapply(libs, library, character.only=TRUE)

survey_dir <- 'survey'
surveys = list(
  ke03 = haven::read_dta(file = file.path(survey_dir, 'KEIR03FL.DTA')),
  ke33 = haven::read_dta(file = file.path(survey_dir, 'KEIR33FL.DTA')),
  ke3a = haven::read_dta(file = file.path(survey_dir, 'KEIR3AFL.DTA')),
  ke42 = haven::read_dta(file = file.path(survey_dir, 'KEIR42FL.DTA')),
  ke52 = haven::read_dta(file = file.path(survey_dir, 'KEIR52FL.DTA')),
  ke71 = haven::read_dta(file = file.path(survey_dir, 'KEIR71FL.DTA'))
) %>% lapply(function(d) { colnames(d) <- tolower(colnames(d)); return(d)})

requirements <- list(
  ke03 = c(case_id = 'caseid', country_phase = 'v000', cluster_id = 'v001',
           weight = 'v005', ever_married = 'v502',
           household_type = 'v102', contraceptive_method = 'v312'),
  ke33 = c(case_id = 'caseid', country_phase = 'v000', cluster_id = 'v001',
           weight = 'v005', ever_married = 'v502', 
           household_type = 'v102', contraceptive_method = 'v312'),
  ke3a = c(case_id = 'caseid', country_phase = 'v000', cluster_id = 'v001',
           weight = 'v005', ever_married = 'v502', 
           household_type = 'v102', contraceptive_method = 'v312'),
  ke42 = c(case_id = 'caseid', country_phase = 'v000', cluster_id = 'v001', 
           weight = 'v005', ever_married = 'v502', 
           household_type = 'v102', contraceptive_method = 'v312'), 
  ke52 = c(case_id = 'caseid', country_phase = 'v000', cluster_id = 'v001', 
           weight = 'v005', ever_married = 'v502', 
           household_type = 'v102', contraceptive_method = 'v312'), 
  ke71 = c(case_id = 'caseid', country_phase = 'v000', cluster_id = 'v001', 
           weight = 'v005', ever_married = 'v502', 
           household_type = 'v102', contraceptive_method = 'v312')
)

data <- list()
for (survey_name in names(surveys)) {
  data[[survey_name]] <- data.frame(survey = rep(survey_name, nrow(surveys[[survey_name]])))
  for (column in requirements[[survey_name]]) {
    if (!(column %in% colnames(surveys[[survey_name]]))) {
      cat("column '", column, "' missing from survey '", survey_name, "'.\n", sep='')
    } else {
      target = names(requirements[[survey_name]])[column == requirements[[survey_name]]]
      data[[survey_name]][[target]] <- self_label(surveys[[survey_name]][[column]])
    }
  }
  data[[survey_name]][['country']] <- get_country(data[[survey_name]][['country_phase']])
  data[[survey_name]][['phase']] <- get_phase(data[[survey_name]][['country_phase']])
  data[[survey_name]][['country_phase']] <- NULL
  data[[survey_name]][['year']] <- kenya_phase_year_map[data[[survey_name]][['phase']]]
}

data <- do.call(what=rbind, args=data) %>% dplyr::mutate(
  uses_modern_method_b = is_modern(contraceptive_method),
  ever_married_b = is_ever_married(ever_married))

saveRDS(data, file = file.path(survey_dir, 'standardized-kenya-surveys.rds'))

model_input_data <- data %>% 
  dplyr::filter(ever_married_b) %>% 
  dplyr::group_by(survey, cluster_id, household_type, country, phase, year) %>% 
  dplyr::summarize(weight = unique(weight), cu_modern = sum(uses_modern_method_b), n = n())


saveRDS(model_input_data, file = 'standardized-survey-data.rds')


