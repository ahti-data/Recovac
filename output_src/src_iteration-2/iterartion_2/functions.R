# Functions

get_breaks_from_year <- function(deaths_dt, kidney_dt, yr=2020) {
  deathsjan <- deaths_dt[
    month1 %in% c(1,2,3,4) & year == yr, unique(rinpersoon)]
  kidjan <- kidney[month1 %in% c(1,2) & year == yr]
  stap <- r_parquet_get_dt(
    glue::glue("H:/data/demog/{yr}/rin_demog.parquet"))
  
  # kidstap <- setdiff(kidjan20, stap20$rinpersoon)
  # deathstap <- setdiff(deathsjan20, stap20$rinpersoon)
  # intersect(kidstap, deathstap)
  
  kidstap <- merge(kidjan, stap, by = "rinpersoon", all = F)
  kidstap <- kidstap[leeftijd > 17]
  
  # Get leeftijdsgroepen
  age_breaks <- quantile(kidstap$leeftijd, probs = seq(0,1, 0.1), na.rm=T)
  print(age_breaks)
  print(table(cut(kidstap$leeftijd, breaks = age_breaks,
                  include.lowest = T)))
  return(age_breaks)
}

compute_death_rates <- function(stapeling_month, deaths_month, 
                                deaths_c_month, deaths_cancer_cvd_month,
                                deaths_resp_month, by_vars) {
  dt <- copy(stapeling_month)
  dt[, `:=` (
    died_all = 0L,
    died_covid = 0L,
    #died_cancer = 0L,
    died_cancer_cvd = 0L,
    #died_cvd = 0L,
    died_resp = 0L
  )]
  dt[.(rinpersoon = deaths_month), on = "rinpersoon", died_all := 1L]
  dt[.(rinpersoon = deaths_c_month), on = "rinpersoon", died_covid := 1L]
  dt[.(rinpersoon = deaths_cancer_cvd_month), on = "rinpersoon", died_cancer_cvd := 1L]
  #dt[.(rinpersoon = deaths_cvd_month), on = "rinpersoon", died_cvd := 1L]
  dt[.(rinpersoon = deaths_resp_month), on = "rinpersoon", died_resp := 1L]
  
  dt[, .(
    all_rate = sum(died_all) / .N,
    covid_rate = sum(died_covid) / .N,
    cancer_cvd_rate = sum(died_cancer_cvd) / .N,
    #cvd_rate = sum(died_cvd) / .N,
    resp_rate = sum(died_resp) / .N,
    N_pop = .N
  ), by = by_vars]
}
  
binomial_sampling_obs_exp <- function(non_kidney_rates, dt_kidney, 
                                     all_cause_kid, covid_kid, cancer_cvd_kid,
                                     resp_kid,
                                     by_vars = c("leeftijd_groep", "geslacht", "seswoa_cat"),
                                     method = "both", # "both", "obs", "exp"
                                     n_samples = 1000,
                                     use_zero_updated_rates=TRUE) {
  # sample expected deaths and observed deaths using binomial distribution.
  
  n_per_profile <- dt_kidney[, .(N = .N), by = by_vars]
  
  # mortality rates for kidney patients
  kidney_rates <- dt_kidney[, .(
    kidney_all_rate = sum(rinpersoon %in% all_cause_kid) / .N,
    kidney_covid_rate = sum(rinpersoon %in% covid_kid) / .N,
    kidney_cancer_cvd_rate = sum(rinpersoon %in% cancer_cvd_kid) / .N,
    #kidney_cvd_rate = sum(rinpersoon %in% cvd_kid) / .N,
    kidney_resp_rate = sum(rinpersoon %in% resp_kid) / .N
  ), by = by_vars]
  
  stats <- merge(non_kidney_rates, kidney_rates, by = by_vars)
  stats <- merge(stats, n_per_profile, by = by_vars)
  #print(stats[1:50])
  
  if (use_zero_updated_rates) {
    print("Updating kidney rates with a single observation from total pop.")
    stats[, kidney_all_rate := (N * kidney_all_rate + all_rate) / (N + 1)]
    stats[, kidney_covid_rate := (N * kidney_covid_rate + covid_rate) / (N + 1)]  
    stats[, kidney_cancer_cvd_rate := (N * kidney_cancer_cvd_rate + cancer_cvd_rate) / (N + 1)] 
    #stats[, kidney_cvd_rate := (N * kidney_cvd_rate + cvd_rate) / (N + 1)] 
    stats[, kidney_resp_rate := (N * kidney_resp_rate + resp_rate) / (N + 1)] 
  }
  
  # Vaste death rates als method niet "both" is
  fixed_obs_allcause <- stats[, sum(N * kidney_all_rate)]
  fixed_obs_covid <- stats[, sum(N * kidney_covid_rate)]
  fixed_obs_cancer_cvd <- stats[, sum(N * kidney_cancer_cvd_rate)]
  #fixed_obs_cvd <- stats[, sum(N * kidney_cvd_rate)]
  fixed_obs_resp <- stats[, sum(N * kidney_resp_rate)]
  
  fixed_exp_allcause <- stats[, sum(N * all_rate)]
  fixed_exp_covid <- stats[, sum(N * covid_rate)]
  fixed_exp_cancer_cvd <- stats[, sum(N * cancer_cvd_rate)]
 # fixed_exp_cvd <- stats[, sum(N * cvd_rate)]
  fixed_exp_resp <- stats[, sum(N * resp_rate)]
  
  res <- replicate(n_samples, {
    stats[, c("new_rate", "new_covid_rate", "new_cancer_cvd_rate",
              "new_resp_rate") := NULL]
    
    obs_allcause <- if (method %in% c("both", "obs")) {
      stats[, sum(rbinom(.N, N, kidney_all_rate))]
    } else{ fixed_obs_allcause }
    
    obs_covid <- if (method %in% c("both", "obs")) {
      stats[, sum(rbinom(.N, N, kidney_covid_rate))]
    } else { fixed_obs_covid }
    
    obs_cancer_cvd <- if (method %in% c("both", "obs")) {
      stats[, sum(rbinom(.N, N, kidney_cancer_cvd_rate))]
    } else { fixed_obs_cancer_cvd }
    
    # obs_cvd <- if (method %in% c("both", "obs")) {
    #   stats[, sum(rbinom(.N, N, kidney_cvd_rate))]
    # } else { fixed_obs_cvd }
    
    obs_resp <- if (method %in% c("both", "obs")) {
      stats[, sum(rbinom(.N, N, kidney_resp_rate))]
    } else { fixed_obs_resp }
    
    exp_allcause <- if (method %in% c("both", "exp")) {
      stats[, new_all_rate := rbinom(.N, N_pop, all_rate) / N_pop]
      stats[, sum(N * new_all_rate)]
    } else { fixed_exp_allcause}
    
    exp_covid <- if (method %in% c("both", "exp")) {
      stats[, new_covid_rate := rbinom(.N, N_pop, covid_rate) / N_pop]
      stats[, sum(N * new_covid_rate)]
    } else {fixed_exp_covid}
    
    exp_cancer_cvd <- if (method %in% c("both", "exp")) {
      stats[, new_cancer_cvd_rate := rbinom(.N, N_pop, cancer_cvd_rate) / N_pop]
      stats[, sum(N * new_cancer_cvd_rate)]
    } else {fixed_exp_cancer_cvd}
    
    # exp_cvd <- if (method %in% c("both", "exp")) {
    #   stats[, new_cvd_rate := rbinom(.N, N_pop, cvd_rate) / N_pop]
    #   stats[, sum(N * new_cvd_rate)]
    # } else {fixed_exp_cvd}
    
    exp_resp <- if (method %in% c("both", "exp")) {
      stats[, new_resp_rate := rbinom(.N, N_pop, resp_rate) / N_pop]
      stats[, sum(N * new_resp_rate)]
    } else {fixed_exp_resp}
    
    
    
    c(smr_allcause = obs_allcause / exp_allcause,
      smr_covid = obs_covid / exp_covid,
      smr_cancer_cvd = obs_cancer_cvd / exp_cancer_cvd,
      #smr_cvd = obs_cvd / exp_cvd,
      smr_resp = obs_resp / exp_resp)
  })
  
  data.table(
    method = method,
    ci_low_allcause = quantile(res["smr_allcause",], 0.025, na.rm=T),
    ci_high_allcause = quantile(res["smr_allcause",], 0.975, na.rm=T),
    
    ci_low_covid = quantile(res["smr_covid",], 0.025, na.rm=T),
    ci_high_covid = quantile(res["smr_covid",], 0.975, na.rm=T),
    
    ci_low_cancer_cvd = quantile(res["smr_cancer_cvd",], 0.025, na.rm=T),
    ci_high_cancer_cvd = quantile(res["smr_cancer_cvd",], 0.975, na.rm=T),

    # ci_low_cvd = quantile(res["smr_cvd",], 0.025, na.rm=T),
    # ci_high_cvd = quantile(res["smr_cvd",], 0.975, na.rm=T),
    
    ci_low_resp = quantile(res["smr_resp",], 0.025, na.rm=T),
    ci_high_resp = quantile(res["smr_resp",], 0.975, na.rm=T)
  )
}


##MDV: moved n_samples declaration into function
##MDV: added mult option
bootstrap_count <- function(dt_sub, deaths_month, n_samples = 30, mult=1) {
  # sample from target pop, count deaths and take mean 
  #'@param mult Option to sample more than one-to-one
  mean(replicate(n_samples, {
    nrow(
      dt_sub[, .SD[sample(.N, N[1] * mult, replace = T)],
             by = .(leeftijd_groep, geslacht, seswoa_cat)][rinpersoon %in% deaths_month]
    ) / mult
  }))
}

# non_kidney_rates <- copy(non_kidney_rates)
# dt_kidney <- copy(demog_dialyse_alive) 
# d_all_cause_kid <- copy(deaths_m)
# d_covid_kid <- copy(deaths_c_rins)

bootstrap_obs_exp <- function(non_kidney_rates, dt_kidney, 
                              d_all_cause_kid, d_covid_kid,
                              by_vars = c("leeftijd_groep", "geslacht", "seswoa_cat"),
                              n_samples = 1000) {
  ##MV: Added seed for reproducibility
  set.seed(1704)
  
  res <- replicate(n_samples, {

    # Bootstrap sample kidney pop
    boots_kidney <- dt_kidney[sample(.N, .N, replace = T)]
    
    n_per_profile <- boots_kidney[, .(N = .N), by = by_vars]
    
    # join death rates of non-kidney population to N per profile from bootstrap sample kidney population
    ##MV: Added all.x=T to ensure we are not missing deaths because of
    ## non-overlap
    boots_stats <- merge(non_kidney_rates, n_per_profile, by= by_vars,
                         all.x=T)
    
    ##MV: Check that all profiles in target group feature in population
    assertthat::assert_that(!any(is.na(boots_stats$N)))
    
    # Observed deaths from bootstrap kidney pop
    obs_allcause <- boots_kidney[rinpersoon %in% d_all_cause_kid, .N]
    obs_covid <- boots_kidney[rinpersoon %in% d_covid_kid, .N]
    
    # Simulate expected deaths by drawing from binom distribution with deaths rates
    # from non-kidney pop and profile sizes from bootstrap kidney pop sample
    exp_allcause <- boots_stats[, sum(rbinom(.N, N, all_rate))]
    exp_covid <- boots_stats[, sum(rbinom(.N, N, covid_rate))]
    
    c(smr_allcause = obs_allcause / exp_allcause,
      smr_covid = obs_covid / exp_covid,
      obs_allcause = obs_allcause,
      exp_allcause = exp_allcause,
      obs_covid = obs_covid,
      exp_covid = exp_covid)
  })
  
  data.table(
    obs_allcause = median(res["obs_allcause",], na.rm = T),
    exp_allcause = median(res["exp_allcause",], na.rm = T),
    smr_all = median(res["smr_allcause",], na.rm = T),
    ci_low_all = quantile(res["smr_allcause", ], 0.025, na.rm = T),
    ci_high_all = quantile(res["smr_allcause", ], 0.975, na.rm = T),
    obs_covid = median(res["obs_covid",], na.rm = T),
    exp_covid = median(res["exp_covid",], na.rm = T),
    smr_covid = median(res["smr_covid",], na.rm = T),
    ci_low_covid = quantile(res["smr_covid", ], 0.025, na.rm = T),
    ci_high_covid = quantile(res["smr_covid", ], 0.975, na.rm = T)
  )
}

##MV: heb de naam van deaths_m verandert naar deaths_month zodat je 100%
## zeker weet dat je niet per ongeluk een deaths_m uit working memory gebruikt

analytical_count <- function(dt, deaths_month, covid_deaths_m, group_count) {
  # Compute (covid) mortality ratio per stratification of non-kidney group
  # Compute expected deaths, for both covid and all-cause, 
  # by kidney group size (N) * mortality rate in non-kidney group
  dt1 <- copy(dt)
  dt1[, `:=` (
    died_all = 0L,
    died_covid = 0L
  )]
  dt1[.(rinpersoon = deaths_month), on = "rinpersoon", died_all := 1L]
  dt1[.(rinpersoon = covid_deaths_m), on = "rinpersoon", died_covid := 1L]
  
  rates <- dt1[, .(
    pop = .N,
    all_deaths = sum(died_all),
    covid_deaths = sum(died_covid),
    all_rate = sum(died_all) / .N,
    covid_rate = sum(died_covid) / .N
  ), by = .(leeftijd_groep, geslacht,seswoa_cat)]
  
  expected <- merge(
    group_count,
    rates[, .(leeftijd_groep, geslacht, seswoa_cat, all_rate, covid_rate)],
    by = c("leeftijd_groep", "geslacht", "seswoa_cat"),
    all.x = T
  )
  
  expected[, `:=`(
    expected_all = N * all_rate,
    expected_covid = N * covid_rate
  )]
  
  return(expected[, .(
    expected_all = sum(expected_all, na.rm = T),
    expected_covid = sum(expected_covid, na.rm = T)
  )])
}