source("src/setup.R")
source("H:/utils/m_functions.R")
library(data.table)

comorbs <- c("rinpersoon", "astma_copd", "diabetes_combi", 
             "hypertensie_chol", "hypertensie_smal", "cholesterol")

# Immuno from the data/aandoeningen file mist een code dus ik maak m zelf
immuno <- c("A07E", "H02A", "H02B", "J06B", "L01A", "L01B", "L01C", "L01D", "L01X",
            "L04A", "M01A", "M01B", "M01C")
med_vars <- c("RINPERSOON", "ATC4")

years <- 2020:2021
dt <- data.table()

for (yr in years) {
  path <- paste0("H:/data/aandoeningen/", yr - 1, "/aandoeningen.parquet")
  print(path)
  aan_dt <- r_parquet_get_dt(path)
  aan_dt[, rinpersoon := as.numeric(rinpersoon)]
  aan_dt <- aan_dt[, ..comorbs]
  aan_dt[, hypertensie_en_chol := as.integer(hypertensie_smal + cholesterol == 2)]
  
  # alleen rijen behouden waar comorb voorkomt
  aan_dt <- aan_dt[rowSums(aan_dt == 1, na.rm = T) > 0]
  
  path <- get_path_newest(
    file.path("G:/GezondheidWelzijn/MEDICIJNTAB",
              yr - 1),
    string_pattern = yr - 1,
    extension = ".csv")
  print(path)
  
  med_dt <- fread(path, select = med_vars)
  med_dt <- format_data(med_dt)
  med_dt <- med_dt[atc4 %chin% immuno]
  med_dt[, immuno := 1]
  
  med_dt <- merge(med_dt, aan_dt, by = "rinpersoon", all = T)
  
  # Save 1 row per person
  per_person <- med_dt[, lapply(.SD, max),
                       by = .(rinpersoon),
                       .SDcols = c("immuno", "diabetes_combi",
                                   "astma_copd", "hypertensie_chol",
                                   "hypertensie_smal", "cholesterol",
                                   "hypertensie_en_chol")]
  setnafill(per_person, fill = 0)
  per_person[, comorbiditeit := 1]
  per_person[, year := yr]
  
  dt <- rbindlist(list(dt, per_person), use.names = T)
}

setindex(dt, NULL)
arrow::write_parquet(dt, "data/raw/comorbidities.parquet")

#meds <- r_parquet_get_dt("data/raw/comorbidities.parquet")
