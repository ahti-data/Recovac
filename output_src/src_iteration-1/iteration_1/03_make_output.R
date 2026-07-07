# Make output


dt <- r_parquet_get_dt("data/results/smr_dt_bootstrap_O_E_final.parquet")
keep_vars <- c("year", "month", "obs_dialyses_deaths_allcause", "smr_obs_dialyses_allcause", 
               "ci_low_dialyse_allcause", "ci_high_dialyse_allcause", 
               "obs_dialyses_deaths_covid","smr_obs_dialyses_covid",       
               "ci_low_dialyse_covid", "ci_high_dialyse_covid", "dialyse_alive",    
               "mort_ratio_dialyse_allcause", "mort_ratio_dialyse_covid", "wave")

output_dt <- dt[, ..keep_vars]

allcause_cols <- grep("allcause", names(output_dt), value =T)
covid_cols <- grep("covid", names(output_dt), value = T)
covid_cols

output_dt[obs_dialyses_deaths_allcause < 10, (allcause_cols) := NA]
output_dt[obs_dialyses_deaths_covid < 10, (covid_cols) := NA]

round_cols <- c("obs_dialyses_deaths_allcause", "obs_dialyses_deaths_covid", "dialyse_alive")
output_dt[, (round_cols) := lapply(.SD, function(x) round (x/5) * 5), .SDcols = round_cols]


setindex(output_dt, NULL)
arrow::write_parquet(output_dt, "data/output/smr_data.parquet")
fwrite(output_dt, "data/output/smr_data.csv")
