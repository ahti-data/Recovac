source("src/setup.R")
source("src/functions.R")
library(data.table)
library(openxlsx)

kidney <- r_parquet_get_dt("data/raw/nefro_year_month.parquet")
kidney20_21 <- kidney[year == 2020 | year == 2021]
deaths <- r_parquet_get_dt("data/raw/deaths_2020_2021.parquet")
tests <- r_parquet_get_dt("data/raw/covid_tests_2020_2021.parquet")
opnames <- r_parquet_get_dt("data/raw/opnames_2020_2021.parquet")
ic_opnames <- opnames[ic == 1]
comorb <- r_parquet_get_dt("data/raw/comorbidities.parquet")
opnames_algemeen <- r_parquet_get_dt("data/raw/non_covid_nefro_opnames.parquet")
opnames_algemeen[, icd10 := NULL]

# neem demog van t jaar daarvoor want deaths in 2020 zitten niet in stapeling 2020
pop20 <- r_parquet_get_dt("H:/data/demog/2019/rin_demog.parquet")
pop20 <- pop20[, .(rinpersoon, geslacht, leeftijd, seswoa_cat)]
pop20[, year := 2020]
pop21 <- r_parquet_get_dt("H:/data/demog/2020/rin_demog.parquet")
pop21 <- pop21[, .(rinpersoon, geslacht, leeftijd, seswoa_cat)]
pop21[, year := 2021]
populatie <- rbindlist(list(pop20, pop21))

rm(pop20, pop21)

# Add comorbidities and non covid/nefro opnames
populatie <- merge(populatie, comorb, by = c("rinpersoon", "year"), all.x = T)
setnafill(populatie, fill = 0, cols = c("immuno","diabetes_combi","astma_copd", 
                                        "hypertensie_chol","hypertensie_smal", 
                                        "cholesterol", "hypertensie_en_chol",
                                        "comorbiditeit"))

populatie <- merge(populatie, opnames_algemeen, by = c("rinpersoon", "year"), 
                   all.x = T)
setnafill(populatie, fill = 0, cols = c("opname_algemeen"))

voeg_event_toe <- function(dt, eventdata, datumkolom, prefix) {
  # functie om sterftes, opnames en positieve testen binnen alle periodes 
  # toe tevoegen
  event_agg <- eventdata[, .(
    p1_2020 = as.integer(any(get(datumkolom) >= as.Date("2020-03-14") & 
                               get(datumkolom) < as.Date("2020-06-15"))),
    p2_2020 = as.integer(any(get(datumkolom) >= as.Date("2020-06-15") & 
                               get(datumkolom) < as.Date("2020-09-16"))),
    p1_2021 = as.integer(any(get(datumkolom) >= as.Date("2021-03-04") & 
                               get(datumkolom) < as.Date("2021-06-05"))),
    p2_2021 = as.integer(any(get(datumkolom) >= as.Date("2021-06-05") & 
                               get(datumkolom) < as.Date("2021-09-06")))
  ), by = rinpersoon]
  
  setnames(event_agg,
           c("p1_2020", "p2_2020", "p1_2021", "p2_2021"),
           paste0(prefix, c("_p1_2020", "_p2_2020", "_p1_2021", "_p2_2021")))
  
  merge(dt, event_agg, by = "rinpersoon", all.x = T)
}

nier_long <- melt(
  kidney20_21,
  id.vars = c("rinpersoon", "year"),
  measure.vars = paste0("month", 1:12),
  variable.name = "maand",
  value.name = "status"
)

nier_long[, maand := as.integer(gsub("month", "", maand))]

nier_status <- dcast(
  nier_long[maand %in% c(3, 6)],
  rinpersoon + year ~ maand,
  value.var = "status"
)

setnames(nier_status, c("3", "6"), c("status_maart", "status_juni"))

dt <- merge(populatie, nier_status, by = c("rinpersoon", "year"), all.x = T)

dt[is.na(status_maart), status_maart := 0L]
dt[is.na(status_juni), status_juni := 0L]


# Merge death to populatie (nodig voor de covid death variabele)
dt <- merge(dt, deaths[, .(rinpersoon, gbadatumoverlijden, covid_death)],
            by = "rinpersoon", all.x = T)

# Add deaths, covid hospitalisation and positive covid tests
dt <- voeg_event_toe(dt, deaths, "gbadatumoverlijden", "dood")
dt <- voeg_event_toe(dt, opnames, "lbzopnamedatum", "opname")
dt <- voeg_event_toe(dt, ic_opnames, "lbzopnamedatum", "ic_opname")
dt <- voeg_event_toe(dt, tests, "datum_besmetting", "positieve_test")

event_cols <- grep("dood|opname|positieve_test", names(dt), value = T)
for (col in event_cols) {
  dt[is.na(get(col)), (col) := 0L]
}

dt[is.na(covid_death), covid_death := 0L]

# Make covid death variable per period
dt[, covid_dood_p1_2020 := covid_death * dood_p1_2020]
dt[, covid_dood_p2_2020 := covid_death * dood_p2_2020]
dt[, covid_dood_p1_2021 := covid_death * dood_p1_2021]
dt[, covid_dood_p2_2021 := covid_death * dood_p2_2021]

# Make "covid death or covid opname" variable per period
dt[, covid_dood_opname_p1_2020 := pmax(covid_dood_p1_2020, opname_p1_2020)]
dt[, covid_dood_opname_p2_2020 := pmax(covid_dood_p2_2020, opname_p2_2020)]
dt[, covid_dood_opname_p1_2021 := pmax(covid_dood_p1_2021, opname_p1_2021)]
dt[, covid_dood_opname_p2_2021 := pmax(covid_dood_p2_2021, opname_p2_2021)]

setindex(dt, NULL)
arrow::write_parquet(dt, "data/raw/regression_data_with_comorb_opname_v2.parquet")
