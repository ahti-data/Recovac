# Descriptives
library(openxlsx)

nierpat <- fread("data/raw/9096_data9096nefrovisieCBKV1.csv")
setDT(nierpat)

transplant_therapie <- c( "TX-postmortaal", "TX-levend", "TX-onbekend")
nierpat <- format_data(nierpat)
nierpat[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

nierpat[jaar == 2020 & maand == 1, .(beide = uniqueN(type) > 1), by = rinpersoon][(beide)]
# median dialyses years
maanden_type1 <- nierpat[type == 1 & jaar < 2020,
                        .(n_maanden = .N),
                        by = rinpersoon]
typjan20 <- nierpat[type == 1 & jaar == 2020 & maand ==1, rinpersoon]

jaren <- maanden_type1[rinpersoon %in% typjan20, n_maanden / 12]
median(jaren)
quantile(jaren, c(0.25,0.5,0.75), na.rm=T)
typjan20 <- nierpat[type == 1 & jaar == 2020 & maand ==1, rinpersoon]

# Check how  many patients were previously transplant
tp <- nierpat[type == 2 & rinpersoon %in% diajan20$rinpersoon]
tpN <- tp[jaar < 2020, uniqueN(rinpersoon)]


# Only keep dialyses patients
nierpat <- nierpat[maand == 1 & jaar == 2020 & type == 1]
nierpat[, geslacht := NULL]

# Read stapeling 
stap20 <- r_parquet_get_dt("H:/data/demog/2019/rin_demog.parquet")
stap20 <- stap20[leeftijd > 17]
stap20[, leeftijd_groep := cut(
  leeftijd,
  breaks = c(18, 42, 51, 57, 61, 65, 69, 72, 76, 81, Inf),
  labels = c("18-41", "42-50", "51-56", "57-60", "61-64", "65-68", "69-71", 
             "72-75", "76-80", "81+"),
  right = F
)]

# telling <- function(dt, by_var) {
#   return(dt[, .N, by = by_var])
# }


# Write stap descriptives to excel
wb <- createWorkbook()

leeftijd <- stap20[, .(
  median_leeftijd = median(leeftijd, na.rm=T),
  iqr25 = quantile(leeftijd, 0.25, na.rm=T),
  iqr75 = quantile(leeftijd, 0.75, na.rm=T)
)]

addWorksheet(wb, "stap_leeftijd")
writeData(wb, "stap_leeftijd", leeftijd)

cat_vars <- c("leeftijd_groep", "seswoa_cat", "geslacht")
long <- melt(stap20, measure.vars = cat_vars, value.name = "categorie")
count_res <- long[, .(n = .N), by = .(variable, categorie)]
count_res[, pct := round(n / sum(n) * 100, 1), by = variable]
rm(long)

addWorksheet(wb, "stap_cat_vars")
writeData(wb, "stap_cat_vars", count_res)



# Make dialyse descriptives and write to excel
diajan20 <- merge(stap20, nierpat, by = "rinpersoon", all = F)
diajan20[, uniqueN(rinpersoon)]
diajan20 <- unique(diajan20, by = "rinpersoon")

doubles <- c(190946041, 364732531, 946755138)

diajan20 <- diajan20[!rinpersoon %in% doubles]

#diajan20[jaar == 2020 & maand ==1, .N, by = rinpersoon][N > 1]

leeftijd_dia <- diajan20[, .(
  median_leeftijd = median(leeftijd, na.rm=T),
  iqr25 = quantile(leeftijd, 0.25, na.rm=T),
  iqr75 = quantile(leeftijd, 0.75, na.rm=T)
)]

addWorksheet(wb, "dia_leeftijd")
writeData(wb, "dia_leeftijd", leeftijd_dia)

cat_vars_dia <- c("leeftijd_groep", "seswoa_cat", "geslacht", "prdcat", "therapie")
long <- melt(diajan20, measure.vars = cat_vars_dia, value.name = "categorie")
count_res <- long[, .(n = .N), by = .(variable, categorie)]
count_res[, pct := round(n / sum(n) * 100, 1), by = variable]
rm(long)

addWorksheet(wb, "dia_cat_vars")
writeData(wb, "dia_cat_vars", count_res)


saveWorkbook(wb, "data/output/descriptives.xlsx", overwrite = T)
