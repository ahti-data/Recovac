source("src/setup.R")

years <- 2020:2021

general_admissions <- function(years){
  # Pak alle opnames op jaar t - 1 behalve covid en nefrologie opnames
  dt <- data.table()
  for (yr in years) {
    path <- get_path_newest(
      file.path("G:/GezondheidWelzijn/LBZBASISTAB", 
                yr - 1), 
      string_pattern=yr - 1,
      extension=".csv")
    print(path)
    
    lbz_dt <- fread(path, select = lbz_vars)
    
    lbz_dt <- format_data(lbz_dt)
    setnames(lbz_dt, "lbzicd10hoofddiagnose", "icd10")
    
    lbz_dt <- lbz_dt[!icd10 %chin% covid_icd10]
    lbz_dt <- lbz_dt[!substr(icd10, 1, 3) %chin% nefro_icd10]
    
    lbz_dt[, opname_algemeen := 1]
    
    #lbz_dt[, year := year(as.Date(as.character(lbzopnamedatum), format = "%Y%m%d"))]
    lbz_dt[, year := yr]
    
    lbz_dt[, c("lbzopnamedatum", "lbzicopnamedag", "lbzicaantaldagen") := NULL]
    
    lbz_dt <- unique(lbz_dt, by = c("rinpersoon", "year"))
    
    dt <- rbindlist(list(dt, lbz_dt), use.names = T)
  }
  return(dt)
}

adm <- general_admissions(years)

setindex(adm, NULL)
arrow::write_parquet(adm, "data/raw/non_covid_nefro_opnames.parquet")
