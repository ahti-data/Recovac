source("src/setup.R")

#prev_test <- r_parquet_get_dt("data/raw/covid_tests.parquet")

covid_tests_file_20 <- "G:/GezondheidWelzijn/GGDCOVID19BM/geconverteerde data/HPZonedata_2020V1.csv"

covid_tests_file_21 <- "G:/GezondheidWelzijn/GGDCOVID19BM/geconverteerde data/HPZonedata_2021V1.csv"
files <- list(covid_tests_file_20, covid_tests_file_21)

test_dt <- data.table()
for (f in files) {
  covid_dt <- format_data(fread(f))
  #covid_dt <- covid_dt[!typeuitslagcovid19test %in% c(4, 5, 8, 9, NA)]
  covid_dt <- covid_dt[rinpersoon != 0 & !is.na(rinpersoon) & rinpersoons == "R"]
  covid_dt[, datum_besmetting := as.Date(tijdstiprapportagecovid19besmetting)]
  #covid_dt[, datum_test := as.Date(as.character(datumcovid19testafname), format = "%Y%m%d")]
  covid_dt[, c("tijdstiprapportagecovid19besmetting", "rinpersoons", "datumcovid19testafname") := NULL]

  covid_dt <- covid_dt[typeuitslagcovid19test %in% c(1, 2)]
  
  if (f == covid_tests_file_20) {
    covid_dt[, year := 2020]
  } else {
    covid_dt[, year := 2021]
  }
  
  covid_dt[, month := month(datum_besmetting)]
  covid_dt[, week := week(datum_besmetting)]
  
  covid_dt <- unique(covid_dt, by = c("rinpersoon", "year", "week"))
  
  test_dt <- rbindlist(list(test_dt, covid_dt), use.names = T)
}

# unique test results per person
test_uitslagen <- test_dt[, .(uitslagen = list(unique(typeuitslagcovid19test))),
                          by = .(rinpersoon, year)]

# Only negatieve uitslagen
alleen_4 <- test_uitslagen[lengths(uitslagen) == 1 & sapply(uitslagen, `[`, 1) == 4]

# Verdeling uitslagen
test_dt[, .N, by = typeuitslagcovid19test]


setindex(test_dt, NULL)
arrow::write_parquet(test_dt, "data/raw/covid_tests_2020_2021.parquet")



# Covid Vaccinations
vacc_dt <- haven::read_sav("G:/GezondheidWelzijn/CIMS/CIMSdata_20260106.sav")
