

d = readRDS("~/data/DHS-kenya/stan-data-env.rds")
m = rstan::read_stan_csv("~/projekty/dhs-user/spatial/kenya-model-output.csv")
mp = rstan::extract(m, permuted=FALSE)
mpp = rstan::extract(m, permuted=TRUE)

county_data = readRDS("~/data/DHS-kenya/kenya-county-data.rds") %>%
  dplyr::select(county_idx, county_name, longitude, latitude) %>%
  rbind(data.frame(county_idx = 48, county_name = "not located", longitude = 40, latitude=-4.12)) 

county_index = county_data[['county_name']]

survey_data = with(data = d, data.frame(county_idx, year_idx, p = n_yes / n_asked)) %>%
  dplyr::mutate(county_label = county_index[county_idx], year = d$year[year_idx],
                county_label_f = factor(county_label, levels=county_index),
                year_label = d$year[year_idx])



rstan::expose_stan_functions(stanmodel='~/projekty/dhs-user/spatial/kenya-model.stan')

gq_indexing = all_permutations(1:max(d$county_idx), 1:(d$n_years + d$n_forecast_years))

o <- list()
for (row_idx in 1:nrow(mpp[['gen_P']])) {
  o[[row_idx]] <- data.frame(
    iteration = row_idx,                             
    county_idx = gq_indexing[[1]],
    year_idx = gq_indexing[[2]],
    x = mpp[['gen_P']][row_idx,]
  )
}

gen_P = do.call(what=rbind, args=o) %>% 
  dplyr::mutate(county_label = county_index[county_idx]) %>%
  dplyr::mutate(county_label_f = factor(county_label, levels=county_index)) %>%
  dplyr::mutate(year_idx_f = factor(d$year[year_idx])) %>%
  dplyr::mutate(year_label = d$year[year_idx])

gen_P_q = gen_P %>% dplyr::group_by(county_idx, county_label_f, year_label) %>%
  dplyr::summarise(` 2.5%` = quantile(x, 0.025), `25.0%` = quantile(x, 0.25),
                   `50.0%` = quantile(x, 0.500), `75.0%` = quantile(x, 0.75),
                   `97.5%` = quantile(x, 0.975)) %>% ungroup()

pl_gen_P = ggplot() +
  geom_line(data = gen_P_q, aes(x=year_label, y=`50.0%`), colour="black") + 
  geom_ribbon(data = gen_P_q, aes(x=year_label, ymin=`25.0%`, ymax=`75.0%`), alpha = 0.5) +
  geom_ribbon(data = gen_P_q, aes(x=year_label, ymin=` 2.5%`, ymax=`97.5%`), alpha = 0.5) +
  geom_point(data = survey_data, aes(x=year_label, y=p)) + 
  facet_wrap(~ county_label_f) +
  scale_y_continuous("proportion of MWRA using modern contraceptive methods") +
  scale_x_continuous("survey year", labels=c(1990, 2005, 2020), breaks=c(1990, 2005, 2020)) +
  theme_minimal()

pdf(file = "~/data/DHS-kenya/kenya-prediction-vs-data.pdf", width=11, height = 22)
print(pl_gen_P); dev.off()

n_counties <- length(county_index)

file_name = paste("~/data/DHS-kenya/kenya-county-prediction-vs-data.pdf")
pdf(file = file_name, width = 6, height = 6)
for (i in 1:n_counties) { 
  partial_gen_P_q = gen_P_q %>% dplyr::filter(county_idx == i) 
  partial_survey_data = survey_data %>% dplyr::filter(county_idx == i)
  pl_gen_c_P = ggplot() +
    geom_line(data = partial_gen_P_q, aes(x=year_label, y=`50.0%`), colour="black") + 
    geom_ribbon(data = partial_gen_P_q, aes(x=year_label, ymin=`25.0%`, ymax=`75.0%`), alpha = 0.5) +
    geom_ribbon(data = partial_gen_P_q, aes(x=year_label, ymin=` 2.5%`, ymax=`97.5%`), alpha = 0.5) +
    geom_jitter(data = partial_survey_data, aes(x=year_label, y=p)) + 
    facet_wrap(~ county_label_f) +
    scale_y_continuous("proportion of MWRA using modern contraceptive methods") +
    scale_x_continuous("survey year", labels=c(1990, 2005, 2020), breaks=c(1990, 2005, 2020)) +
    theme_minimal()
  print(pl_gen_c_P)
}
dev.off()

o <- list()
for (row_idx in 1:nrow(mpp[['design_theta']])) {
  o[[row_idx]] <- data.frame(
    iteration = row_idx, 
    county_idx = d$sampled_unit_idx,
    year_idx = d$sampled_year_idx,
    x = mpp[['design_theta']][row_idx,]
  )
}

design_theta = do.call(what=rbind, args=o) %>% 
  dplyr::mutate(county_label = county_index[county_idx]) %>%
  dplyr::mutate(county_label_f = factor(county_label, levels=county_index)) %>%
  dplyr::mutate(year_idx_f = factor(d$year[year_idx])) %>%
  dplyr::mutate(year_label = d$year[year_idx])

design_theta_q = design_theta %>% dplyr::group_by(county_idx, county_label_f, year_label) %>%
  dplyr::summarise(` 2.5%` = quantile(x, 0.025), `25.0%` = quantile(x, 0.250),
                   `50.0%` = quantile(x, 0.500), `75.0%` = quantile(x, 0.750),
                   `97.5%` = quantile(x, 0.975)) %>% ungroup()


pl_gen = ggplot() +
  geom_line(data = gen_P_q, aes(x=year_label, y=`50.0%`), colour="black") + 
  geom_ribbon(data = gen_P_q, aes(x=year_label, ymin=`25.0%`, ymax=`75.0%`), alpha = 0.5) +
  geom_ribbon(data = gen_P_q, aes(x=year_label, ymin=` 2.5%`, ymax=`97.5%`), alpha = 0.5) +
  geom_point(data = design_theta_q, aes(x=year_label, y=`50.0%`)) + 
  geom_errorbar(data = design_theta_q, aes(x=year_label, ymin=`25.0%`, ymax=`50.0%`)) + 
  geom_errorbar(data = design_theta_q, aes(x=year_label, ymin=` 2.5%`, ymax=`97.5%`)) + 
  facet_wrap(~ county_label_f) +
  scale_y_continuous("proportion of MWRA using modern contraceptive methods") +
  scale_x_continuous("survey year", labels=c(1990, 2005, 2020), breaks=c(1990, 2005, 2020)) +
  theme_minimal()

pdf(file = "~/data/DHS-kenya/kenya-prediction-vs-estimates.pdf", width=11, height = 22)
print(pl_gen); dev.off()

n_counties <- length(county_index)

file_name = paste("~/data/DHS-kenya/kenya-county-prediction-vs-estimates.pdf")
pdf(file = file_name, width = 6, height = 6)
for (i in 1:n_counties) { 
  partial_gen_P_q = gen_P_q %>% dplyr::filter(county_idx == i) 
  partial_design_theta_q = design_theta_q %>% dplyr::filter(county_idx == i)
  partial_survey_data = survey_data %>% dplyr::filter(county_idx == i)
  pl_gen_c = ggplot() +
    geom_line(data = partial_gen_P_q, aes(x=year_label, y=`50.0%`), colour="black") + 
    geom_ribbon(data = partial_gen_P_q, aes(x=year_label, ymin=`25.0%`, ymax=`75.0%`), alpha = 0.5) +
    geom_ribbon(data = partial_gen_P_q, aes(x=year_label, ymin=` 2.5%`, ymax=`97.5%`), alpha = 0.5) +
    geom_jitter(data = partial_survey_data, aes(x=year_label, y=p), alpha = 0.5, color = 'yellow') + 
    geom_point(data = partial_design_theta_q, aes(x=year_label, y=`50.0%`)) + 
    geom_errorbar(data = partial_design_theta_q, 
                  aes(x=year_label, ymin=`25.0%`, ymax=`75.0%`), color='grey') + 
    geom_errorbar(data = partial_design_theta_q, aes(x=year_label, ymin=` 2.5%`, ymax=`97.5%`)) + 
    facet_wrap(~ county_label_f) +
    scale_y_continuous("proportion of MWRA using modern contraceptive methods") +
    scale_x_continuous("survey year", labels=c(1990, 2005, 2020), breaks=c(1990, 2005, 2020)) +
    theme_minimal()
  print(pl_gen_c)
  file_name = paste0("~/data/DHS-kenya/kenya-", county_index[i], "-prediction-vs-estimates.pdf")
  pdf(file = file_name, width = 6, height = 6)
  print(pl_gen_c); dev.off()
}
dev.off()





