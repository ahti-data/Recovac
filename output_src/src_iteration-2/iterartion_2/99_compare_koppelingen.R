library(openxlsx)

nierpat1 <- fread("data/raw/9096_data9096nefrovisieCBKV1.csv")
setDT(nierpat1)

nierpat2 <- fread("data/raw/9096_data9096nefrovisieCBKV2.csv")
setDT(nierpat2)

nierpat3 <- fread("data/raw/9096_data9096nefrovisieCBKV3.csv")
setDT(nierpat3)

nierpat_fetch <- r_parquet_get_dt("H:/_Current_projects/recovac/data/nierpatienten/nierpatienten.parquet")
nierpat_fetch[, rinpersoon := as.numeric(rinpersoon)]

transplant_therapie <- c( "TX-postmortaal", "TX-levend", "TX-onbekend")
nierpat1 <- format_data(nierpat1)
nierpat1[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]
nierpat2 <- format_data(nierpat2)
nierpat2[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]
nierpat3 <- format_data(nierpat3)
nierpat3[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

# Create workbook
wb <- createWorkbook()

# Aantal gekoppelde rins per koppeling en type nierpatient
kop1 <- nierpat1[, .(rins = uniqueN(rinpersoon), 
                     fpn = uniqueN(fictief_patnr_crypt)),
                 by = type]
kop1[, koppeling := 1]

kop2 <- nierpat2[, .(rins = uniqueN(rinpersoon), 
                     fpn = uniqueN(fictief_patnr_crypt)), by = type]
kop2[, koppeling := 2]

kop3 <- nierpat3[, .(rins = uniqueN(rinpersoon), 
                     fpn = uniqueN(fictief_patnr_crypt)), by = type]
kop3[, koppeling := 3]

koppelingen <- rbindlist(list(kop1, kop2, kop3))
koppelingen[, perc := round(rins/fpn * 100,2)]
koppelingen[, type := fifelse(type ==2, "ktr", "dialyse")]


addWorksheet(wb, "vergelijk koppelingen")
writeData(wb, "vergelijk koppelingen", koppelingen)

# Rins wel in koppeling 2 maar niet in koppeling 1
rinsk1 <- nierpat1[!is.na(rinpersoon), unique(rinpersoon)]

rinsk2 <- nierpat2[!is.na(rinpersoon), unique(rinpersoon)]

rin_diff <- setdiff(rinsk2, rinsk1)

years <- 2016:2023
msz_vars <- c("RINPERSOON", "VEKTMSZSpecialismeDiagnoseCombinatie", 
              "VEKTMSZDBCZorgproduct", "VEKTMSZInstellingPrest", 
              "VEKTMSZBegindatumPrest")

kidney_patients <- function(years){
  dt <- data.table()
  for (yr in years) {
    print(yr)
    msz_path <- get_path_newest("G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/geconverteerde data",
                                string_pattern = glue::glue("MSZPrestatiesVEKT{yr}"),
                                extension = ".parquet")
    #print(msz_path)
    
    msz_file <- arrow::read_parquet(msz_path, col_select = all_of(msz_vars))
    setDT(msz_file)
    
    msz_file <- msz_file[RINPERSOON %in% rin_diff]
    msz_file <- format_data(msz_file)
    
    msz_file[, spec := substr(msz_file$vektmszspecialismediagnosecombinatie, 1, 4)]
    msz_file[, diag := substr(msz_file$vektmszspecialismediagnosecombinatie, 12, 15)]
    
    msz_file[, jaar := yr]
    msz_file[, maand := lubridate::month(lubridate::ymd(vektmszbegindatumprest))]
    
    dt <- rbindlist(list(dt, msz_file), use.names = T)
    rm(msz_file)
  }
  return(dt)
}

res <- kidney_patients(years)

#es[, .N, by = .(spec, diag)][order(-N)][1:50]

#top50 <- res[, .(N = uniqueN(rinpersoon)), by = .(spec, diag)][order(-N)][1:50]
#res_test[, .(N = uniqueN(rinpersoon)), by = .(spec, diag)][order(-N)][1:30]

# Make data tables for rinpersonen that are in hulpbestand and K2 en not in hulpbestand but in K2
overlap_fetch_k2 <- intersect(nierpat_fetch$rinpersoon, rin_diff)
diff_fetch_k2 <- setdiff(rin_diff,nierpat_fetch$rinpersoon)

data_overlap <- res[rinpersoon %in% overlap_fetch_k2]
data_overlap <- merge(data_overlap, nierpat2, by = c("rinpersoon", "jaar", "maand"), all.x=T)
data_overlap[is.na(type), type := 0]

data_diff <- res[rinpersoon %in% diff_fetch_k2]
data_diff <- merge(data_diff, nierpat2, by = c("rinpersoon", "jaar", "maand"), all.x=T)
data_diff[is.na(type), type := 0]

#overlap_spec_diag <- data_overlap[type == 0, .(N = uniqueN(rinpersoon)), by = .(spec, diag)][order(-N)][1:50]
overlap_spec_diag_rins <- data_overlap[, .(N = uniqueN(rinpersoon)), 
                                  by = .(type, spec, diag)][order(type, -N), head(.SD, 20), by = type]
overlap_spec_diag_totaal <- data_overlap[, .N, 
                                  by = .(type, spec, diag)][order(type, -N), head(.SD, 20), by = type]
overlap_spec_diag_pp <- data_overlap[, .N, 
                                         by = .(type, spec, diag, rinpersoon)][order(type, -N), head(.SD, 20), by = type]

#diff_spec_diag <- data_diff[, .(N = uniqueN(rinpersoon)), by = .(spec, diag, type)][order(-N)][1:100]
diff_spec_diag_rins <- data_diff[, .(N = uniqueN(rinpersoon)), 
                                  by = .(type, spec, diag)][order(type, -N), head(.SD, 30), by = type]
diff_spec_diag_totaal <- data_diff[, .N, 
                            by = .(type, spec, diag)][order(type, -N), head(.SD, 30), by = type]
diff_spec_diag_pp <- data_diff[, .N, 
                                   by = .(type, spec, diag, rinpersoon)][order(type, -N), head(.SD, 30), by = type]

interne_geneeskunde <- data_diff[spec == "0313", uniqueN(rinpersoon)]


addWorksheet(wb, "In HB, unieke rins per dbc")
writeData(wb, "In HB, unieke rins per dbc", overlap_spec_diag_rins)

addWorksheet(wb, "In HB, N per dbc")
writeData(wb, "In HB, N per dbc", overlap_spec_diag_totaal)

addWorksheet(wb, "In HB, N per dbc per rin")
writeData(wb, "In HB, N per dbc per rin", overlap_spec_diag_pp)

addWorksheet(wb, "niet in HB, unieke rins per dbc")
writeData(wb, "niet in HB, unieke rins per dbc", diff_spec_diag_rins)

addWorksheet(wb, "niet in HB, N per dbc")
writeData(wb, "niet in HB, N per dbc", diff_spec_diag_totaal)

addWorksheet(wb, "niet in HB, N per dbc per rin")
writeData(wb, "niet in HB, N per dbc per rin", diff_spec_diag_pp)


# addWorksheet(wb, "spec_diag")
# writeData(wb, "spec_diag", "In HB en nieuw in K2 unieke rinpersoon per dbc", startCol = 1, startRow = 1)
# writeData(wb, "spec_diag", overlap_spec_diag_rins, startCol = 1, startRow = 2)
# writeData(wb, "spec_diag", "In HB en nieuw in K2 N per dbc", startCol = ncol(overlap_spec_diag_rins) + 2, startRow = 1)
# writeData(wb, "spec_diag", overlap_spec_diag_totaal, startCol = ncol(overlap_spec_diag_rins) + 2, startRow = 2)
# 
# 
# writeData(wb, "spec_diag", "Niet in onze lijst en nieuw in K2", startCol = ncol(overlap_spec_diag) + 2, startRow = 1)
# writeData(wb, "spec_diag", diff_spec_diag, startCol = ncol(overlap_spec_diag) + 2, startRow = 2)


# Waar wonen mensen die we missen in K1
woonplaats <- function(years){
  dt <- data.table()
  for (yr in years) {
    print(yr)
  
    stap_path <- get_path_newest(
      file.path("H:/data/demog", 
                yr), 
      string_pattern=yr,
      extension=".parquet",
      method= "newest")
    #print(stap_path)
    
    stapeling <- r_parquet_get_dt(stap_path)
    
    stapeling <- stapeling[rinpersoon %in% rin_diff]
    stapeling[, year := yr]
    
    dt <- rbindlist(list(dt, stapeling), use.names = T, fill = T)
  }
  return(dt)
}

woon_dt <- woonplaats(years)

gem <- woon_dt[, .(N = uniqueN(rinpersoon)), by = gem][order(-N)][1:50]
gem

addWorksheet(wb, "gemeentes")
writeData(wb, "gemeentes", "Gemeentes mensen wel in K2 niet in K1", startCol = 1, startRow = 1)
writeData(wb, "gemeentes", gem, startCol = 1, startRow = 2)


saveWorkbook(wb, "data/koppeling_check_uitgebreid.xlsx", overwrite = T)
