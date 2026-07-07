## Check: Mark
## Date: 01-04-2026

source("src/setup.R")

covid_admissions <- function(years){
  
  dt <- data.table()
  for (yr in years) {
    path <- get_path_newest(
      file.path("G:/GezondheidWelzijn/LBZBASISTAB", 
                yr), 
      string_pattern=yr,
      extension=".csv")
    print(path)
    
    lbz_dt <- fread(path, select = lbz_vars)
    
    
    ## MV: We kiezen hier om alleen de hoofddiagnose mee te nemen.
    ## Je kan ook de geimputeerde meenemen (die is alleen gevuld als hoofd
    ## diagnose NA is). In principe rapporteert Stateline zelf de gecombineerde
    ## versie (dus if(!is.na(hoofd), hoofd, imputed)) dus zij vinden het iig
    ## legit genoeg. Zou voor ons wel weer wat extra N geven.
    ### TS: Recovac wil specifiek alleen naar U071 kijken
    lbz_dt <- format_data(lbz_dt)
    setnames(lbz_dt, "lbzicd10hoofddiagnose", "icd10")
    
    lbz_dt <- lbz_dt[icd10 %chin% c("U071")]#, "U072")]
    
    lbz_dt[, lbzopnamedatum := as.Date(as.character(lbzopnamedatum), format = "%Y%m%d")]
    
    lbz_dt[, year := yr]
    lbz_dt[, month := month(lbzopnamedatum)]
    lbz_dt[, week := week(lbzopnamedatum)]
    lbz_dt[, ic := fifelse(lbzicaantaldagen != 0, 1, 0)]
    
    lbz_dt <- unique(lbz_dt, by = c("rinpersoon", "year", "week"))
    
    
    dt <- rbindlist(list(dt, lbz_dt), use.names = T)
  }
  return(dt)
}

adm <- covid_admissions(2020:2021)

setindex(adm, NULL)
arrow::write_parquet(adm, "data/raw/opnames_2020_2021.parquet")
