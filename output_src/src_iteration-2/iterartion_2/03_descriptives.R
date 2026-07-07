# Descriptives
library(openxlsx)

dialyse_pat <- fread("data/raw/9096_data9096nefrovisieCBKV1.csv")
setDT(dialyse_pat)

ktr_pat <- fread("data/raw/9096_data9096nefrovisieCBKV3.csv")
setDT(ktr_pat)

transplant_therapie <- c( "TX-postmortaal", "TX-levend", "TX-onbekend")

dialyse_pat <- format_data(dialyse_pat)
dialyse_pat[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

ktr_pat <- format_data(ktr_pat)
ktr_pat[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

dialyse_pat[jaar == 2020 & maand == 1, .(beide = uniqueN(type) > 1), by = rinpersoon][(beide)]

# median dialyses years
# dialyse_pat[is.na(totaal_ddag), totaal_ddag := 0]
# dialyse_pat[, jaren_dia_voor_2016 := totaal_ddag / 365]
# 
# maanden_type1 <- dialyse_pat[!is.na(rinpersoon) & type == 1 & jaar < 2020,
#                         .(n_maanden = .N),
#                         by = rinpersoon]
# maanden_type1[, aantal_jaar := n_maanden / 12]
# 
# 
# typjan20 <- dialyse_pat[!is.na(rinpersoon) & type == 1 & jaar == 2020 & maand ==1]
# 
# jaren <- maanden_type1[rinpersoon %in% typjan20, n_maanden / 12]
# median(jaren)
# quantile(jaren, c(0.25,0.5,0.75), na.rm=T)
# typjan20 <- nierpat[type == 1 & jaar == 2020 & maand ==1, rinpersoon]



# Check how  many patients were previously transplant
tp <- nierpat[type == 2 & rinpersoon %in% diajan20$rinpersoon]
tpN <- tp[jaar < 2020, uniqueN(rinpersoon)]


# Only keep dialyses patients
nipat <- nierpat[maand == 1 & jaar == 2020 & type == 1]
nierpat[, geslacht := NULL]

# Read stapeling 
stap20 <- r_parquet_get_dt("H:/data/demog/2019/rin_demog.parquet")
#stap20 <- stap20[leeftijd > 17]
stap20[, leeftijd_groep := cut(
  leeftijd,
  breaks = c(-Inf, 19, 44, 64, 74, Inf),
  labels = c("0-19", "20-44", "45-64", "65-74", "75+"),
  right = T
)]

stap20[, leeftijd_small := cut(
  leeftijd,
  breaks = c(-Inf, 17, 60, 74, 84, Inf),
  labels = c("0-17", "18-60", "61-74", "74-84", "85+"),
  right = T
)]

# Write stap descriptives to excel
wb <- createWorkbook()

leeftijd <- stap20[, .(
  median_leeftijd = median(leeftijd, na.rm=T),
  iqr25 = quantile(leeftijd, 0.25, na.rm=T),
  iqr75 = quantile(leeftijd, 0.75, na.rm=T)
)]

addWorksheet(wb, "stap_leeftijd")
writeData(wb, "stap_leeftijd", leeftijd)

cat_vars <- c("leeftijd_groep", "leeftijd_small", "seswoa_cat", "geslacht")
long <- melt(stap20, measure.vars = cat_vars, value.name = "categorie")
count_res <- long[, .(n = .N), by = .(variable, categorie)]
count_res[, pct := round(n / sum(n) * 100, 1), by = variable]
count_res <- count_res[order(variable, categorie)]
rm(long)

addWorksheet(wb, "stap_cat_vars")
writeData(wb, "stap_cat_vars", count_res)



# Make dialyse descriptives and write to excel
# ZORGEn dat dit alle nierpatienten zijn die wij ook echt meenemen
dialyse_pat <- dialyse_pat[maand == 1 & jaar == 2020 & type == 1]
dialyse_pat[, geslacht := NULL]
diajan20 <- merge(stap20, dialyse_pat, by = "rinpersoon", all = F)

diajan20[, uniqueN(rinpersoon)]
diajan20 <- unique(diajan20, by = "rinpersoon")

#doubles <- c(190946041, 364732531, 946755138)
diajan20 <- diajan20[!rinpersoon %in% double_therapie_rins]

#diajan20[jaar == 2020 & maand ==1, .N, by = rinpersoon][N > 1]

leeftijd_dia <- diajan20[, .(
  median_leeftijd = median(leeftijd, na.rm=T),
  iqr25 = quantile(leeftijd, 0.25, na.rm=T),
  iqr75 = quantile(leeftijd, 0.75, na.rm=T)
)]
addWorksheet(wb, "dia_leeftijd")
writeData(wb, "dia_leeftijd", leeftijd_dia)

cat_vars_dia_all_ages <- c("leeftijd_groep", "geslacht")
long <- melt(diajan20, measure.vars = cat_vars_dia_all_ages, value.name = "categorie")
count_res <- long[, .(n = .N), by = .(variable, categorie)]
count_res[, pct := round(n / sum(n) * 100, 1), by = variable]
rm(long)

addWorksheet(wb, "dia_cat_vars_alle_leeftijden")
writeData(wb, "dia_cat_vars_alle_leeftijden", count_res)


diajan20 <- diajan20[leeftijd >= 18]
cat_vars_dia <- c("leeftijd_small", "seswoa_cat", "geslacht", "prdcat", "therapie")
long <- melt(diajan20, measure.vars = cat_vars_dia, value.name = "categorie")
count_res <- long[, .(n = .N), by = .(variable, categorie)]
count_res[, pct := round(n / sum(n) * 100, 1), by = variable]
rm(long)

addWorksheet(wb, "dia_cat_vars")
writeData(wb, "dia_cat_vars", count_res)


# Aantal jaar dia voor jan 2020
diajan20[is.na(totaal_ddag), totaal_ddag := 0]
diajan20[, jaar_dia_voor_2016 := totaal_ddag / 365]

dia_pat1 <- fread("data/raw/9096_data9096nefrovisieCBKV1.csv")
dia_pat1 <- format_data(dia_pat1)
dia_pat1[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]
dia_pat1 <- dia_pat1[jaar %in% c(2016,2017,2018,2019)]
dia_pat1 <- dia_pat1[type == 1]
dia_rijen_pp <- dia_pat1[!is.na(rinpersoon), .N, by = rinpersoon]
dia_rijen_pp[, aantal_jaar := N / 12]

diajan20 <- merge(diajan20, dia_rijen_pp, by = "rinpersoon", all.x = T)
diajan20[, totaal_jaar := rowSums(.SD, na.rm=T), .SDcols = c("jaar_dia_voor_2016",
                                                             "aantal_jaar")]

jaren_dia <- diajan20[, .(
  median_jaren_dia = median(totaal_jaar, na.rm=T),
  iqr25 = quantile(totaal_jaar, 0.25, na.rm=T),
  iqr75 = quantile(totaal_jaar, 0.75, na.rm=T)
)]

addWorksheet(wb, "jaren_dia")
writeData(wb, "jaren_dia", jaren_dia)



#### KTR descriptives ####
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



ktr_pat <- ktr_pat[maand == 1 & jaar == 2020 & type == 2]
ktr_pat[, geslacht := NULL]
ktrjan20 <- merge(stap20, ktr_pat, by = "rinpersoon", all = F)

ktrjan20[, uniqueN(rinpersoon)]
ktrjan20 <- unique(ktrjan20, by = "rinpersoon")

ktrjan20 <- ktrjan20[!rinpersoon %in% double_therapie_rins]

ktrjan20 <- ktrjan20[rinpersoon %in% stap20_ktr$rinpersoon]
#ktrjan20[leeftijd > 17, .N]

cat_vars_ktr_all_ages <- c("leeftijd_groep", "geslacht")
long <- melt(ktrjan20, measure.vars = cat_vars_ktr_all_ages, value.name = "categorie")
count_res <- long[, .(n = .N), by = .(variable, categorie)]
count_res[, pct := round(n / sum(n) * 100, 1), by = variable]
rm(long)

addWorksheet(wb, "ktr_cat_vars_alle_leeftijden")
writeData(wb, "ktr_cat_vars_alle_leeftijden", count_res)


ktrjan20 <- ktrjan20[leeftijd >= 18]
cat_vars_ktr <- c("leeftijd_small", "seswoa_cat", "geslacht", "prdcat", "therapie")
long <- melt(ktrjan20, measure.vars = cat_vars_ktr, value.name = "categorie")
count_res <- long[, .(n = .N), by = .(variable, categorie)]
count_res[, pct := round(n / sum(n) * 100, 1), by = variable]
rm(long)

addWorksheet(wb, "ktr_cat_vars")
writeData(wb, "ktr_cat_vars", count_res)

leeftijd_ktr <- ktrjan20[, .(
  median_leeftijd = median(leeftijd, na.rm=T),
  iqr25 = quantile(leeftijd, 0.25, na.rm=T),
  iqr75 = quantile(leeftijd, 0.75, na.rm=T)
)]
addWorksheet(wb, "ktr_leeftijd")
writeData(wb, "ktr_leeftijd", leeftijd_ktr)


names(ktrjan20)
ktrjan20[is.na(totaal_txdag), totaal_txdag := 0]
ktrjan20[, jaar_ktr_voor_2016 := totaal_txdag / 365]

ktr_pat1 <- fread("data/raw/9096_data9096nefrovisieCBKV3.csv")
ktr_pat1 <- format_data(ktr_pat1)
ktr_pat1[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]
ktr_pat1 <- ktr_pat1[jaar %in% c(2016,2017,2018,2019)]
ktr_pat1 <- ktr_pat1[type == 2]
ktr_rijen_pp <- ktr_pat1[!is.na(rinpersoon), .N, by = rinpersoon]
ktr_rijen_pp[, aantal_jaar := N / 12]

ktrjan20 <- merge(ktrjan20, ktr_rijen_pp, by = "rinpersoon", all.x = T)
ktrjan20[, totaal_jaar := rowSums(.SD, na.rm=T), .SDcols = c("jaar_ktr_voor_2016",
                                                             "aantal_jaar")]

jaren_ktr <- ktrjan20[, .(
  median_jaren_ktr = median(totaal_jaar, na.rm=T),
  iqr25 = quantile(totaal_jaar, 0.25, na.rm=T),
  iqr75 = quantile(totaal_jaar, 0.75, na.rm=T)
)]

addWorksheet(wb, "jaren_ktr")
writeData(wb, "jaren_ktr", jaren_ktr)



saveWorkbook(wb, "data/output/descriptives_iteratie2.xlsx", overwrite = T)
