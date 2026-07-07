nierpat <- fread("data/raw/9096_data9096nefrovisieCBKV3.csv")
setDT(nierpat)

nier_data <- r_parquet_get_dt("data/raw/nefro_year_month_k1_k3.parquet")

transplant_therapie <- c( "TX-postmortaal", "TX-levend", "TX-onbekend")
nierpat <- format_data(nierpat)
nierpat[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

jan2020 <- nierpat[type == 2 & maand == 1 & jaar == 2020]
jan2020[, uniqueN(fictief_patnr_crypt)]
jan2020[!is.na(rinpersoon), uniqueN(rinpersoon)]

nier_data_ktr_jan_20 <- nier_data[year == 2020 & month1 == 2]
nier_data_ktr_jan_20

stap20 <- r_parquet_get_dt("H:/data/demog/2019/rin_demog.parquet")

stap20_ktr <- stap20[rinpersoon %in% nier_data_ktr_jan_20$rinpersoon]
stap20_ktr[leeftijd > 17 & !rinpersoon %in% double_therapie_rins , .N]
