source("src/setup.R")

# Do some basic counts

deaths <- r_parquet_get_dt("data/raw/death_per_month_year.parquet")

# Only keep rows of year someone actually dies
death_filter <- deaths[Reduce(`|`, lapply(deaths, function(x) x == 1 | x == 2 | x == 3 | x == 4))]

kidney <-  r_parquet_get_dt("data/raw/nefro_year_month.parquet")
# N kidney patients per month
# N deaths per month
# N kidney deaths per month
# N dialyses deaths per month
# N transplant deaths per month
years <- 2020:2022
months <- 1:12

n <- length(months) * length(years)

deaths_dt <- data.table(
  year = numeric(n),
  month = numeric(n),
  
  deaths = numeric(n),
  
  cov_deaths = numeric(n),
  # cov_deaths_men = nrow(men_c),
  # cov_deaths_women = nrow(women_c),
  
  kidney_deaths = numeric(n),
  kidney_cov_deaths_basic = numeric(n),
  
  kidney_cov_deaths_extra = numeric(n),
  kidney_cov_deaths_total = numeric(n),
  
  dia_cov_deaths_extra = numeric(n)
  # # deaths_men = numeric(n),
  # # deaths_women = numeric(n),
  # 
  # cov_deaths = numeric(n),
  # # cov_deaths_men = numeric(n),
  # # cov_deaths_women = numeric(n),
  # 
  # 
  # kidney_deaths = numeric(n),
  # kidney_cov_deaths = numeric(n),
  # kidney_cov_extra_deaths  = numeric(n)
)

# Covid deaths per month
i <- 1

for (yr in years) {
  
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
  
  if (yr == 2022) {
    stapeling[, c("leeftijd_0tm17", "leeftijd_18_plus") := NULL]
  }
  # Keep "gem" in for possible later plotting of spatial outcomes
  stapeling[, c("veiligheidsregio", "zorgkantoorregio", "bc", "gem", "wc", "hbopl",
                "gbaherkomstgroepering", "huishoudsamenstelling", "herkomst3",
                "herkomst7", "inkomen_klasse", "seswoa_cat", "leeftijd") := NULL]
  
  death_filter_dt <- death_filter[year == yr]
  
  kidney_yr <- kidney[year == yr]
  
  death_sex <- merge(death_filter_dt, stapeling, all.x=T, by = "rinpersoon")
  
  
  for (m in months) {
    mcol <- sprintf("month%d",m)
    
    deaths_m <- death_filter_dt[get(mcol) %in% c(1,2,3,4), unique(rinpersoon)]
    #deaths_m <- death_sex[get(mcol) %in% c(1,2,3,4)]
    # men <- deaths_m[geslacht == "Mannen"]
    # women <- deaths_m[geslacht == "Vrouwen"]
    deaths_c_basic <- death_filter_dt[get(mcol) == 2, unique(rinpersoon)]
    #deaths_c_basic <- death_sex[get(mcol) == 2]
    # men_c <- deaths_c[geslacht == "Mannen"]
    # women_c <- deaths_c[geslacht == "Vrouwen"]
    
    deaths_c_extra <- death_filter_dt[get(mcol) %in% c(3,4), unique(rinpersoon)]
    
    deaths_c_total <- death_filter_dt[get(mcol) %in% c(2,3,4), unique(rinpersoon)]
    
    kidney_m <- kidney_yr[get(mcol) %in% c(1,2), .(rinpersoon, type = get(mcol))]
    kidney_deaths <- kidney_m[rinpersoon %in% deaths_m]
    dialyse_deaths <- nrow(kidney_deaths[type ==1])
    transplant_deaths <- nrow(kidney_deaths[type ==2])
    
    kidney_cov_deaths_basic = kidney_deaths[rinpersoon %in% deaths_c_basic, .N]
    #print(kidney_cov_deaths_basic)
    kidney_cov_deaths_ext = kidney_deaths[rinpersoon %in% deaths_c_extra, .N]
    print(kidney_cov_deaths_ext)
    kidney_cov_deaths_total = kidney_deaths[rinpersoon %in% deaths_c_total, .N]
    
    
    dialyses_cov_deaths_ext = kidney_deaths[type == 1 & rinpersoon %in% deaths_c_extra, .N]
    transplant_cov_deaths_ext = kidney_deaths[type == 2 & rinpersoon %in% deaths_c_extra]
    
    
    # kidney_deaths_a <- merge(deaths_m, kidney_yr, by = "rinpersoon", all = F)
    # kidney_deaths_c <- merge(deaths_c_basic, kidney_yr, by = "rinpersoon", all = F)
    # 
    # kidney_deaths_c_ext <- merge(deaths_c_extra, kidney_yr, by = "rinpersoon", 
    #                              all = F)
    # 
    # kidney_deaths_c_total <- merge(deaths_c_total, kidney_yr, by = "rinpersoon", 
    #                              all = F)
    # 
    # kid_deaths1 <- deaths_c[rinpersoon %in% kidney_yr$rinpersoon]
    
    deaths_dt[i, `:=` (
      year = yr,
      month = m,
      
      deaths = NROW(deaths_m),
      # deaths_men = nrow(men),
      # deaths_women = nrow(women),
      
      cov_deaths = NROW(deaths_c_total),
      # cov_deaths_men = nrow(men_c),
      # cov_deaths_women = nrow(women_c),
      
      kidney_deaths = NROW(kidney_deaths),
      kidney_cov_deaths_basic = kidney_cov_deaths_basic,
      
      kidney_cov_deaths_extra = kidney_cov_deaths_ext,
      kidney_cov_deaths_total = kidney_cov_deaths_total,
      
      dia_cov_deaths_extra = dialyses_cov_deaths_ext
    )]
    
    i <- i + 1
  }
}

setindex(deaths_dt, NULL)
arrow::write_parquet(deaths_dt, "H:/_Current_projects/recovac/data/raw/deaths(cov_kidney)_permonthyear.parquet")

# Patienten dbc counts

kidney <- r_parquet_get_dt("H:/_Current_projects/recovac/data/raw/patients_dbcs_dates.parquet")

kidney[, uniqueN(rinpersoon), by = .(year,diag)][order(diag)]
kidney[, uniqueN(rinpersoon), by = .(year)]


# COVID admissions
adm <- covid_admissions(2020:2023) 

adm[, .N, by = year]


# Tests
tests <- r_parquet_get_dt("data/raw/covid_tests.parquet")
tests[, month := month(datum_besmetting)]

test_dt[, .N, 
      by = .(year, month)][order(year, month)]
