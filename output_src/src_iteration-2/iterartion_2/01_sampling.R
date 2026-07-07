source("src/setup.R")
source("src/functions.R")
library(ggplot2)
library(glue)

version <- "dia_ktr_cvd_cancer_samen"
method <- "both"

deaths <- r_parquet_get_dt("data/raw/death_per_month_year_all_death_causes.parquet")
#kidney <- r_parquet_get_dt("data/raw/patient_per_month_and_year.parquet")
kidney <- r_parquet_get_dt("data/raw/nefro_year_month_k1_k3.parquet")
kidney <- kidney[!rinpersoon %in% double_therapie_rins]
years <- c(2016:2023)
months <- 1:12

#age_breaks <- get_breaks_from_year(deaths, kidney, yr=2020)

by_vars <- c("leeftijd_groep", "geslacht", "seswoa_cat")

n <- length(months) * length(years)

smr_dt <- data.table(
  year = numeric(n)
  # month = numeric(n),
)

#smr_dt_ses <- data.table()

i <- 1
# yr <- 2021
tictoc::tic("Computing SMRs")
for (yr in years) {
  # Take year - 1 for demog data because we are looking at deaths in a year
  yr_min1 <- yr -1
  stap_path <- get_path_newest(
    file.path("H:/data/demog", 
              yr_min1), 
    string_pattern=yr_min1,
    extension=".parquet",
    method= "newest")
  print(stap_path)
  
  # Read and process stapeling
  # TIME: 12-14 seconds
  stapeling <- r_parquet_get_dt(stap_path)
  stapeling <- stapeling[leeftijd > 17]
  
  stapeling[, seswoa_group := fcase(
   seswoa_cat == "0-10%", "laag",
   seswoa_cat == "10-20%", "laag",
   seswoa_cat == "20-35%", "midden-laag",
   seswoa_cat == "35-50%", "midden-laag",
   seswoa_cat == "50-75%", "midden-hoog",
   seswoa_cat == "75-100%", "hoog",
   seswoa_cat == "Onbekend", "onbekend"
  )]
  
  # if (yr == 2022) {
  #   stapeling[, c("leeftijd_0tm17", "leeftijd_18_plus") := NULL]
  # }
  # Keep "gem" in for possible later plotting of spatial outcomes
  stapeling[, c("veiligheidsregio", "zorgkantoorregio", "bc", "wc", "hbopl",
                "gbaherkomstgroepering", "huishoudsamenstelling", "herkomst3",
                "herkomst7", "inkomen_klasse") := NULL]
  
  # Make age groups 
  #age_breaks <- get_breaks_from_year(deaths, kidney, yr=yr)
  stapeling[, leeftijd_groep := cut(
    leeftijd,
    breaks = c(18, 42, 51, 57, 61, 65, 69, 72, 76, 81, Inf),
    labels = c("18-41", "42-50", "51-56", "57-60", "61-64", "65-68", "69-71", 
               "72-75", "76-80", "81+"),
    right = F
  )]
  stapeling[, leeftijd_groep_small := cut(
    leeftijd,
    breaks = c(18, 51, 61, 70, Inf),
    labels = c("18-50", "51-60", "61-69", "70+"),
    right = F
  )]
  
  setkey(stapeling, rinpersoon)
  
  # filtering deaths and kidney patients for year "yr"
  death_dt <- deaths[year == yr]
  kidney_dt <- kidney[year == yr]
  
  # Only use deaths of people we have demog data of and are 18+
  death_stapeling <- merge(death_dt, stapeling, all = F, by = "rinpersoon")
  
  alive_rins <- stapeling$rinpersoon
  
  ## Ensure all deaths are findable in stapeling to get demog chars from
  assertthat::assert_that(all(
    death_stapeling$rinpersoon %in% stapeling$rinpersoon))
  
  for (m in months) {
    mcol <- sprintf("month%d",m)
    #mcol <- "month1"
    message("computing month: ", m, " and year: ", yr)
    
    # Alive population
    # TIME: ~ 6 seconds
    #stapeling_m <- stapeling[rinpersoon %in% alive_rins] # use indexing 
    
    # TIME: ~ 4 seconds
    stapeling_m <- stapeling[J(alive_rins)]
    
    # Deaths in month m
    # Make this dt to get geslacht/age of people who died that month
    deaths_stap_m <- death_stapeling[get(mcol) %in% c(1,2,3,4,5,6,99)]
    deaths_covid_stap_m <- death_stapeling[get(mcol) == 2]
    
    deaths_m <- deaths_stap_m[, unique(rinpersoon)]
    
    # Deaths per death cause
    #deaths_nc <- deaths_stap_m[get(mcol) == 1]
    deaths_c <- deaths_stap_m[get(mcol) == 2] # covid deaths
    deaths_c_rins <- deaths_c$rinpersoon
    
    # deaths_pres_c <- deaths_stap_m[get(mcol) == 3]
    # deaths_pres_c_rins <- deaths_pres_c$rinpersoon
    
    deaths_cancer <- deaths_stap_m[get(mcol) == 4]
    deaths_cancer_rins <- deaths_cancer$rinpersoon
    
    deaths_cvd <- deaths_stap_m[get(mcol) == 5]
    deaths_cvd_rins <- deaths_cvd$rinpersoon
    
    deaths_cancer_cvd_rins <- union(deaths_cancer_rins, deaths_cvd_rins)
    
    deaths_resp <- deaths_stap_m[get(mcol) == 6]
    deaths_resp_rins <- deaths_resp$rinpersoon
  
    
    # Kidney patients in month m
    kidney_m <- kidney_dt[get(mcol) %in% c(1,2), .(rinpersoon, type = get(mcol))]
    
    #dialyse_m <- kidney_dt[get(mcol) == 1, .(rinpersoon)]
    #ktr_m <- kidney_dt[get(mcol) == 2, .(rinpersoon)]
    
    # general kidney deaths in month m
    kidney_deaths <- kidney_m[rinpersoon %in% deaths_m]
    dialyse_deaths <- nrow(kidney_deaths[type ==1])
    transplant_deaths <- nrow(kidney_deaths[type ==2])
    
    # Kidney deaths per disease in month m
    # covid
    kidney_cov_deaths = kidney_deaths[rinpersoon %in% deaths_c_rins]
    dialyses_cov_deaths = kidney_cov_deaths[type == 1]
    transplant_cov_deaths = kidney_cov_deaths[type == 2]
    
    # cancer
    kidney_cancer_deaths = kidney_deaths[rinpersoon %in% deaths_cancer_rins]
    dialyses_cancer_deaths = kidney_cancer_deaths[type == 1]
    transplant_cancer_deaths = kidney_cancer_deaths[type == 2]
    
    # cvd
    kidney_cvd_deaths = kidney_deaths[rinpersoon %in% deaths_cvd_rins]
    dialyses_cvd_deaths = kidney_cvd_deaths[type == 1]
    transplant_cvd_deaths = kidney_cvd_deaths[type == 2]
    
    # cancer or cvd
    kidney_cancer_cvd_deaths = kidney_deaths[rinpersoon %in% deaths_cancer_cvd_rins]
    dialyses_cancer_cvd_deaths = kidney_cancer_cvd_deaths[type == 1]
    transplant_cancer_cvd_deaths = kidney_cancer_cvd_deaths[type == 2]
    
    # resp
    kidney_resp_deaths = kidney_deaths[rinpersoon %in% deaths_resp_rins]
    dialyses_resp_deaths = kidney_resp_deaths[type == 1]
    transplant_resp_deaths = kidney_resp_deaths[type == 2]
    
    # some checks
    assertthat::assert_that(
      all(c(dialyses_cov_deaths$rinpersoon,
            transplant_cov_deaths$rinpersoon) %in%
            kidney_cov_deaths$rinpersoon)
    )
    
    assertthat::assert_that(
      all(c(dialyses_resp_deaths$rinpersoon,
            transplant_resp_deaths$rinpersoon) %in%
            kidney_resp_deaths$rinpersoon)
    )
    
    # Alive kidney patients in month m. 
    # TIME: ~ 3 seconds
    #kidney_alive <- kidney_m[rinpersoon %in% alive_rins] # does the same as line below but slower
    # TIME: ~ 1.5 seconds
    kidney_alive <- kidney_m[.(rinpersoon = alive_rins), on = "rinpersoon", nomatch = 0]
    
    # Demog of alive kidney patients
    # TIME: ~1.5 seconds
    #demog_kidney_alive <- stapeling_m[rinpersoon %in% kidney_alive$rinpersoon] # does the same as line below but slower
    # TIME: 0 seconds
    demog_kidney_alive <- stapeling_m[kidney_alive, on = "rinpersoon", nomatch = 0]
    demog_dialyse_alive <- demog_kidney_alive[type == 1]
    demog_transplant_alive <- demog_kidney_alive[type == 2]
    
    print(assertthat::assert_that(
      nrow(demog_kidney_alive) == length(kidney_alive$rinpersoon)
    ))
    
    # Counts kidney patients per group
    # TIME: 0 seconds
    group_counts <- demog_kidney_alive[, .N, 
                                       by = .(type, leeftijd_groep, geslacht, seswoa_cat)]
    
    group_counts_dialyse <- group_counts[type == 1]
    group_counts_transplant <- group_counts[type == 2]
    
    # Alive non-kidney patients
    # TIME: 1.2-2 seconds
    #stap_non_kidney_m <- stapeling_m[!rinpersoon %in% kidney_alive$rinpersoon]
    
    # TIME: 1.2 seconds
    stap_non_kidney_m <- stapeling_m[!kidney_alive, on = "rinpersoon"]
    
    #stap_deaths_non_kidney <- stap_non_kidney_m[rinpersoon %in% deaths_m]
    
    print("join counts")
    
    ##MV: Ik heb toegevoegd dat we all.y=T forcen op de merge en verifieren
    ## dat we voor elke groep in de group_counts ook minstens een persoon vinden
    ## in de stap_non_kidney_m. Gewoon om zeker te zijn dat er neit een persoon
    ## is in de kidney groep waarvan er geen non-kidney versie is (is niet zo)
    
    # Add respective stratification group size to every rin
    # TIME: 2.5-3 seconds
    # SKIP this step when using O hat and E hat outcomes, because strat group size
    # changes every sample
    # dt_join_dialyse <- merge(
    #   stap_non_kidney_m,
    #   group_counts_dialyse,
    #   by = c("leeftijd_groep", "geslacht", "seswoa_cat"),
    #   all.y = T
    # )
    # 
    # assertthat::assert_that(
    #   !any(is.na(dt_join_dialyse$rinpersoon))
    # )
    # 
    # 
    # dt_join_transplant <- merge(
    #   stap_non_kidney_m,
    #   group_counts_transplant,
    #   by = c("leeftijd_groep", "geslacht", "seswoa_cat"),
    #   all.y = T
    # )
    
    # 
    # assertthat::assert_that(
    #   !any(is.na(dt_join_transplant$rinpersoon))
    # )
    
    # Compute death rates for non-kidney patients per profile
    # dt1 <- copy(stap_non_kidney_m)
    # dt1[, `:=` (
    #   died_all = 0L,
    #   died_covid = 0L
    # )]
    # dt1[.(rinpersoon = deaths_m), on = "rinpersoon", died_all := 1L]
    # dt1[.(rinpersoon = deaths_c_rins), on = "rinpersoon", died_covid := 1L]
    # 
    # non_kidney_rates <- dt1[, .(
    #   #pop = .N,
    #   #all_deaths = sum(died_all),
    #   #covid_deaths = sum(died_covid),
    #   all_rate = sum(died_all) / .N,
    #   covid_rate = sum(died_covid) / .N
    # ), by = .(leeftijd_groep, geslacht, seswoa_cat)]
    
    
    # LET OP BY VARS
    # non_kidney_rates <- compute_death_rates(stap_non_kidney_m, deaths_m,
    #                                         deaths_c_rins, deaths_cancer_rins,
    #                                         deaths_cvd_rins, deaths_resp_rins,
    #                                         by_vars = by_vars)
    
    non_kidney_rates <- compute_death_rates(stap_non_kidney_m, 
                                            deaths_m,
                                            deaths_c_rins, 
                                            deaths_cancer_cvd_rins,
                                            #deaths_cvd_rins,
                                            deaths_resp_rins,
                                            by_vars = by_vars)
    
    
    # 
    # non_kidney_rates_ses <- compute_death_rates(stap_non_kidney_m, deaths_m, 
    #                                             deaths_c_rins, 
    #                                             by_vars = c("leeftijd_groep", "geslacht"))
    
    
    # tictoc::tic("add leeftijd/geslacht group size")
    # TIME: ~2.5 seconds
    # dt_join_new <- stap_non_kidney_m[group_counts, on = .(leeftijd_groep, geslacht, seswoa_cat), nomatch = 0]
    # tictoc::toc()
    # 
    # print(all.equal(dt_join, dt_join_new))
    
    # reproducible sample
    #set.seed(123) 
    
    # Draw samples from alive non-kidney patients according to group_counts
    #samples <- list()
    
    # non_kidney_deaths_dialyse <- bootstrap_obs_exp(non_kidney_rates, demog_dialyse_alive, 
    #                                                deaths_m, deaths_c_rins,
    #                                                by_vars = c("leeftijd_groep", "geslacht", "seswoa_cat"),
    #                                                n_samples = 1000)
    
    ## Population-estimated dialysis deaths
    # demog_dialyse_alive_n <- demog_dialyse_alive[
    #   , .(n_kidney = .N), by = by_vars]
    # 
    # 
    # demog_dialyse_alive_n_with_rates <- merge(
    #   demog_dialyse_alive_n, non_kidney_rates,
    #   all.x=T, by = by_vars
    # )
    
    #### Compute "E" part of SMR points estimates ####
    # Expected dialysis deaths
    demog_dialyse_alive_n_with_rates <- merge(
      group_counts_dialyse, non_kidney_rates,
      all.x=T, by = by_vars
    )
    expected_dialysis_deaths <- sum(demog_dialyse_alive_n_with_rates$all_rate *
                                      demog_dialyse_alive_n_with_rates$N)
    expected_covid_dialysis_deaths <- sum(demog_dialyse_alive_n_with_rates$covid_rate *
                                            demog_dialyse_alive_n_with_rates$N)
    expected_cancer_cvd_dialysis_deaths <- sum(demog_dialyse_alive_n_with_rates$cancer_cvd_rate *
                                            demog_dialyse_alive_n_with_rates$N)
    # expected_cvd_dialysis_deaths <- sum(demog_dialyse_alive_n_with_rates$cvd_rate *
    #                                         demog_dialyse_alive_n_with_rates$N)
    expected_resp_dialysis_deaths <- sum(demog_dialyse_alive_n_with_rates$resp_rate *
                                            demog_dialyse_alive_n_with_rates$N)
    
    # Expected transplant deaths
    demog_transplant_alive_n_with_rates <- merge(
      group_counts_transplant, non_kidney_rates,
      all.x=T, by = by_vars
    )
    expected_transplant_deaths <- sum(demog_transplant_alive_n_with_rates$all_rate *
                                        demog_transplant_alive_n_with_rates$N)
    expected_covid_transplant_deaths <- sum(demog_transplant_alive_n_with_rates$covid_rate *
                                              demog_transplant_alive_n_with_rates$N)
    expected_cancer_cvd_transplant_deaths <- sum(demog_transplant_alive_n_with_rates$cancer_cvd_rate *
                                              demog_transplant_alive_n_with_rates$N)
    # expected_cvd_transplant_deaths <- sum(demog_transplant_alive_n_with_rates$cvd_rate *
    #                                           demog_transplant_alive_n_with_rates$N)
    expected_resp_transplant_deaths <- sum(demog_transplant_alive_n_with_rates$resp_rate *
                                              demog_transplant_alive_n_with_rates$N)
    
    print("sampling")
    tictoc::tic("Sampling")
    #### binomial sampling ####
    # Dialysis 
    non_kidney_deaths_dialyse <- binomial_sampling_obs_exp(
      non_kidney_rates,
      demog_dialyse_alive,
      deaths_m,
      deaths_c_rins,
      deaths_cancer_cvd_rins,
      #deaths_cvd_rins,
      deaths_resp_rins,
      by_vars = by_vars,
      method = method,
      n_samples = 1000,
      use_zero_updated_rates = T
    )
    
    # Transplant
    non_kidney_deaths_transplant <- binomial_sampling_obs_exp(
      non_kidney_rates,
      demog_transplant_alive,
      deaths_m,
      deaths_c_rins,
      deaths_cancer_cvd_rins,
      #deaths_cvd_rins,
      deaths_resp_rins,
      by_vars = by_vars,
      method = method,
      n_samples = 1000,
      use_zero_updated_rates = T
    )
    
    #### SES ####
    
    # Do here in case there is no ses group in a certain month
    #ses_groups <- unique(demog_dialyse_alive$seswoa_group)

    # res_ses_dialyse <- rbindlist(lapply(ses_groups, function(ses) {
    #   res <- bootstrap_obs_exp(non_kidney_rates_ses,
    #                            demog_dialyse_alive[seswoa_group == ses],
    #                            deaths_stap_m[seswoa_group == ses, rinpersoon],
    #                            deaths_covid_stap_m[seswoa_group == ses, rinpersoon],
    #                            by_vars = c("leeftijd_groep", "geslacht"),
    #                            n_samples = 1000)
    #   res[, `:=` (seswoa_group = ses, type = "dialyse", year = yr, month = m)]
    #   res
    # }))
    # 
    # smr_dt_ses <- rbindlist(list(smr_dt_ses, res_ses_dialyse))
    # 
    
    # non_kidney_deaths_transplant <- bootstrap_obs_exp(non_kidney_rates, demog_transplant_alive, 
    #                                                deaths_m, deaths_c_rins,
    #                                                n_samples = 10)
    
    # non_kidney_deaths_dialyse <- analytical_count(dt_join_dialyse, deaths_m, 
    #                                               deaths_c_rins, group_counts_dialyse)  
    # 
    # non_kidney_deaths_transplant <- analytical_count(dt_join_transplant, deaths_m,
    #                                                  deaths_c_rins, group_counts_transplant) 
    tictoc::toc()
    
    # Update population at risk
    # TIME: ~ 2 seconds
    alive_rins <- setdiff(alive_rins, deaths_m)
    
    # Save deaths among kidney and non-kidney patients per month
    # TIME: 0 seconds
    
    causes <- c("allcause", "covid", "cancer_cvd")#, "resp") # cvd
    obs_dialyse <- setNames(list(dialyse_deaths, nrow(dialyses_cov_deaths), 
                                 nrow(dialyses_cancer_cvd_deaths)), causes) #nrow(dialyses_resp_deaths)
    obs_transplant <- setNames(list(transplant_deaths, nrow(transplant_cov_deaths), 
                                    nrow(transplant_cancer_cvd_deaths)), causes)
    exp_dialyse <- setNames(list(expected_dialysis_deaths, expected_covid_dialysis_deaths,
                                 expected_cancer_cvd_dialysis_deaths), causes)
    exp_transplant <- setNames(list(expected_transplant_deaths, expected_covid_transplant_deaths,
                                    expected_cancer_cvd_transplant_deaths), causes)
    
    smr_dt[i, `:=` (
      year = yr,
      month = m,
      total_alive_start = nrow(stapeling_m),
      total_deaths = nrow(deaths_stap_m),
      total_covid_deaths = nrow(deaths_c),
      total_kidney_deaths = nrow(kidney_deaths),
      kidney_covid_deaths = nrow(kidney_cov_deaths),
      
      dialyse_alive = nrow(kidney_alive[type == 1]),
      mort_ratio_dialyse_allcause = (round(dialyse_deaths / 5) * 5) / nrow(kidney_alive[type == 1]),
      mort_ratio_dialyse_covid = (round(nrow(dialyses_cov_deaths) / 5) * 5) / nrow(kidney_alive[type == 1]),
      mort_ratio_dialyse_cancer_cvd = (round(nrow(dialyses_cancer_cvd_deaths) / 5) * 5) / nrow(kidney_alive[type == 1]),
      #mort_ratio_dialyse_cvd = (round(nrow(dialyses_cvd_deaths) / 5) * 5) / nrow(kidney_alive[type == 1]),
      #mort_ratio_dialyse_resp = (round(nrow(dialyses_resp_deaths) / 5) * 5) / nrow(kidney_alive[type == 1]),
      
      transplant_alive = nrow(kidney_alive[type == 2]),
      mort_ratio_transplant_allcause = (round(transplant_deaths / 5) * 5) / nrow(kidney_alive[type == 2]),
      mort_ratio_transplant_covid = (round(nrow(transplant_cov_deaths) / 5) * 5) / nrow(kidney_alive[type == 2]),
      mort_ratio_transplant_cancer = (round(nrow(transplant_cancer_cvd_deaths) / 5) * 5) / nrow(kidney_alive[type == 2])
      #mort_ratio_transplant_cvd = (round(nrow(transplant_cvd_deaths) / 5) * 5) / nrow(kidney_alive[type == 2]),
      #mort_ratio_transplant_resp = (round(nrow(transplant_resp_deaths) / 5) * 5) / nrow(kidney_alive[type == 2])
    )]
    
    for (cause in causes) {
      d_obs <- obs_dialyse[[cause]]
      t_obs <- obs_transplant[[cause]]
      d_exp <- exp_dialyse[[cause]]
      t_exp <- exp_transplant[[cause]]
      
      smr_dt[i, paste0(c(
        "obs_dialyse_", "exp_dialyse_", "smr_dialyse_",
        "ci_low_dialyse_", "ci_high_dialyse_", "ci_diff_dialyse_",
        "obs_transplant_", "exp_transplant_", "smr_transplant_",
        "ci_low_transplant_", "ci_high_transplant_",
        "ci_diff_transplant_"), cause) := list(
          d_obs, d_exp, round(d_obs / d_exp, 2),
          round(non_kidney_deaths_dialyse[[paste0("ci_low_", cause)]], 2),
          round(non_kidney_deaths_dialyse[[paste0("ci_high_", cause)]], 2),
          round((non_kidney_deaths_dialyse[[paste0("ci_high_", cause)]] - 
                   non_kidney_deaths_dialyse[[paste0("ci_low_", cause)]]) / 2, 3),
          t_obs, t_exp, round(t_obs / t_exp, 2),
          round(non_kidney_deaths_transplant[[paste0("ci_low_", cause)]], 2),
          round(non_kidney_deaths_transplant[[paste0("ci_high_", cause)]], 2),
          round((non_kidney_deaths_transplant[[paste0("ci_high_", cause)]] - 
                   non_kidney_deaths_transplant[[paste0("ci_low_", cause)]]) / 2, 3)
        )]
    }
    i <- i + 1
  }
}
tictoc::toc()


smr_dt[, date := as.Date(sprintf("%d-%02d-01", year, month))]

smr_dt[, wave := fcase(
  year == 2020 & month %in% c(3,4,5,6), "lockdown1",
  year == 2020 & month %in% c(7,8,9), "inter-lockdown",
  year == 2020 & month %in% c(10,11,12), "lockdown2",
  year == 2021 & month %in% c(1,2,3,4), "lockdown2",
  year == 2021 & month > 4, "after vacc & delta",
  year == 2022, "omicron",
  default = "no covid period"
)]

setindex(smr_dt, NULL)
arrow::write_parquet(
  smr_dt, glue("data/results/smr_iteratie2/smr_dt_bootstrap_{method}_final{version}.parquet"))
fwrite(
  smr_dt, glue("data/results/smr_iteratie2/smr_dt_bootstrap_{method}_final{version}.csv"))
writexl::write_xlsx(
  smr_dt, glue("data/results/smr_iteratie2/smr_dt_bootstrap_{method}_final{version}.xlsx"))



#smr_dt[, diff_smr_cih := ci_high_dialyse_allcause - smr_obs_hat_dialyses_allcause_mean]

ggplot(smr_dt) +
  # geom_point(aes(y=smr_dt$smr_obs_dialyses_allcause, x = smr_dt$date),
  #            color = 'green') +
  # geom_line(aes(y=smr_dt$ci_high_dialyse_allcause, x = smr_dt$date)) +
  # geom_line(aes(y=smr_dt$ci_low_dialyse_allcause, x = smr_dt$date)) +
  # geom_point(aes(y=smr_dt$smr_dialyse_allcause, x = smr_dt$date),
  #            color = 'blue') +
  # 
  # geom_line(aes(y=smr_dt$ci_high_transplant_allcause, x = smr_dt$date)) +
  # geom_line(aes(y=smr_dt$ci_low_transplant_allcause, x = smr_dt$date)) +
  # geom_point(aes(y=smr_dt$smr_transplant_allcause, x = smr_dt$date),
  #            color = 'red') +
  
  geom_line(aes(y=smr_dt$ci_high_dialyse_cvd, x = smr_dt$date)) +
  geom_line(aes(y=smr_dt$ci_low_dialyse_cvd, x = smr_dt$date)) +
  geom_point(aes(y=smr_dt$smr_dialyse_cvd, x = smr_dt$date),
             color = 'green') +
  # geom_point(aes(y=smr_dt$smr_obs_hat_dialyses_allcause_mean, x = smr_dt$date),
  #            color = 'red') +
  # geom_line(aes(y= abs(smr_dt$ci_high_dialyse_allcause - smr_dt$smr_obs_hat_dialyses_allcause_mean), 
  #               x = smr_dt$date), color = "purple") +
  # geom_line(aes(y= abs(smr_dt$ci_low_dialyse_allcause - smr_dt$smr_obs_hat_dialyses_allcause_mean), 
  #               x = smr_dt$date), color = "orange") +
  # geom_line(aes(y = (abs(smr_dt$ci_high_dialyse_allcause - smr_dt$smr_obs_dialyses_allcause_raw) -
  #                 abs(smr_dt$ci_low_dialyse_allcause - smr_dt$smr_obs_dialyses_allcause_raw)),
  #               x = smr_dt$date)) +
                  
  # geom_point(aes(y=smr_dt$smr_obs_hat_dialyses_allcause, x = smr_dt$date),
  #            color = 'lightgreen') +
  theme_bw()


print(sum(abs((abs(smr_dt$ci_high_dialyse_allcause - smr_dt$smr_obs_dialyses_allcause_raw) -
         abs(smr_dt$ci_low_dialyse_allcause - smr_dt$smr_obs_dialyses_allcause_raw)))))


ggplot(smr_dt1) +
  # geom_point(aes(y=smr_dt$smr_obs_dialyses_allcause, x = smr_dt$date),
  #            color = 'green') +
  geom_line(aes(y=smr_dt1$ci_high_dialyse_allcause, x = smr_dt1$date)) +
  geom_line(aes(y=smr_dt1$ci_low_dialyse_allcause, x = smr_dt1$date)) +
  # geom_point(aes(y=smr_dt$smr_obs_hat_dialyses_allcause_mean, x = smr_dt$date),
  #            color = 'red') +
  # geom_line(aes(y= abs(smr_dt$ci_high_dialyse_allcause - smr_dt$smr_obs_hat_dialyses_allcause_mean), 
  #               x = smr_dt$date), color = "purple") +
  # geom_line(aes(y= abs(smr_dt$ci_low_dialyse_allcause - smr_dt$smr_obs_hat_dialyses_allcause_mean), 
  #               x = smr_dt$date), color = "orange") +
  # geom_line(aes(y = (abs(smr_dt1$ci_high_dialyse_allcause - smr_dt1$smr_obs_dialyses_allcause_raw) -
  #                      abs(smr_dt1$ci_low_dialyse_allcause - smr_dt1$smr_obs_dialyses_allcause_raw)),
  #               x = smr_dt1$date)) +
  
  # geom_point(aes(y=smr_dt$smr_obs_hat_dialyses_allcause, x = smr_dt$date),
  #            color = 'lightgreen') +
  geom_point(aes(y=smr_dt$smr_obs_dialyses_allcause_raw, x = smr_dt$date),
             color = 'blue') +
  theme_bw()




ggplot(smr_dt) +
  # geom_point(aes(y=smr_dt$smr_obs_transplant_allcause, x = smr_dt$date),
  #            color = 'green') +
  geom_line(aes(y=smr_dt$ci_high_transplant_allcause, x = smr_dt$date)) +
  geom_line(aes(y=smr_dt$ci_low_transplant_allcause, x = smr_dt$date)) +
  geom_point(aes(y=smr_dt$smr_obs_hat_transplant_allcause_mean, x = smr_dt$date),
             color = 'red', alpha = .4) +
  geom_point(aes(y=smr_dt$smr_obs_hat_transplant_allcause, x = smr_dt$date),
             color = 'blue', alpha = .4) +
  geom_point(aes(y=smr_dt$smr_obs_transplant_allcause_raw, x = smr_dt$date),
             color = 'yellow') +
  theme_bw()

# setindex(smr_dt_ses, NULL)
# arrow::write_parquet(
#   smr_dt_ses, glue("data/results/smr_dt_bootstrap_SESWOA{version}.parquet"))
# fwrite(smr_dt_ses, glue("data/results/smr_dt_bootstrap_SESWOA{version}.csv"))
#death_count_dt <- r_parquet_get_dt("H:/recovac/data/raw/death_count_per_month_year.parquet")
#smr1 <- r_parquet_get_dt("data/results/smr_dt_analytical_2019_2023_age_gender_ses.parquet")
#ddc <- r_parquet_get_dt("data/raw/deaths_dates_causes.parquet")