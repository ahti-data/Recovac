# Check nefrovisie data

nierpat <- fread("data/raw/9096_data9096nefrovisieCBKV1.csv")
setDT(nierpat)
# nierpat_vars <- c("geboortedatum", "overlijdensdatum", "pcode4", "geslacht", "lftmaand",
#                   "therapie", "jaar", "maand", "RINPersoon")
#nierpat <- nierpat[, ..nierpat_vars]
transplant_therapie <- c( "TX-postmortaal", "TX-levend", "TX-onbekend")
nierpat <- format_data(nierpat)

nierpat[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

# FLOW DIAGRAM NUMBERS
dia_pop_nefro <- nierpat[type == 1 & maand == 1 & jaar == 2020, uniqueN(fictief_patnr_crypt)]
dia_pop_nefro

dia_pop_rin <- nierpat[type == 1 & maand == 1 & jaar == 2020, uniqueN(rinpersoon)]
dia_pop_rin


count_types <- nierpat[, uniqueN(rinpersoon), by = .(therapie, jaar)]
count_types#[, V1]
cat(count_types[, V1], sep = "\n")

# COUNT DIA en TRANS PER JAAR
count_types <- nierpat[, uniqueN(fictief_patnr_crypt), by = .(type, jaar)]
#count_types[, cor := V1 / 0.94]
count_types
#cat(count_types[, V1], sep = "\n")

# DEATHS PER year
nierpat[, overlijdjaar := substr(overlijdensdatum, 1, 4)]
nierpat[, uniqueN(fictief_patnr_crypt), by = .(overlijdjaar)][order(overlijdjaar)]

count_types <- nierpat[, uniqueN(fictief_patnr_crypt), by = .(therapie, jaar)]
count_types
cat(count_types[, V1], sep = "\n")


beiden <- nierpat[, .(both = all(c(1,2) %in% type)), by = .(fictief_patnr_crypt, jaar)][both == T]
filt <- nierpat[beiden, on = .(fictief_patnr_crypt, jaar)]
filt[, .N, by = .(therapie, jaar)][order(jaar)]



# for all matches, check if uniqueN fictier nr == uniqueN rinpersoon
dt1 <- nierpat[!is.na(rinpersoon)]
dt1[, uniqueN(fictief_patnr_crypt)]
dt1[, uniqueN(rinpersoon)]


# Checke duplicates
dupe <- nierpat[, if (.N > 1) .SD, by = .(rinpersoon, jaar, maand)]
dupe_diff_therapie <- dupe[, if (uniqueN(therapie) > 1) .SD, by = .(rinpersoon, jaar, maand)]

setindex(dupe_diff_therapie, NULL)
arrow::write_parquet(dupe_diff_therapie, "data/raw/kidney_pat_doubles.parquet")


# Check non-matches

non_match <- nierpat[is.na(rinpersoon)]
n_nm <- non_match[, uniqueN(fictief_patnr_crypt)]
n_nm
n_nm_dead <- non_match[!is.na(overlijdensdatum), uniqueN(fictief_patnr_crypt)]
n_nm_dead
non_match[!is.na(overlijdensdatum), .(N = uniqueN(fictief_patnr_crypt), perc = uniqueN(fictief_patnr_crypt) / n_nm * 100), by = .(therapie)]
non_match[totaal_txdag > 0 & therapie %chin% c("TX-postmortaal", "TX-levend", "TX-onbekend"), mean(totaal_txdag)]

non_match[totaal_txdag != 0, uniqueN(fictief_patnr_crypt)]


non_match[, .(gem_lft = mean(lftmaand), gem_gesl = mean(geslacht)), by = type]


nierpat[!is.na(rinpersoon), .(gem_lft = mean(lftmaand), gem_gesl = mean(geslacht)), by = type]




