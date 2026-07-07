# P-values

nierpat <- fread("data/raw/9096_data9096nefrovisieCBKV1.csv")
# nierpat_vars <- c("geboortedatum", "overlijdensdatum", "pcode4", "geslacht", "lftmaand",
#                   "therapie", "jaar", "maand", "RINPersoon")
#nierpat <- nierpat[, ..nierpat_vars]
transplant_therapie <- c( "TX-postmortaal", "TX-levend", "TX-onbekend")
nierpat <- format_data(nierpat)
nierpat <- nierpat[!is.na(rinpersoon)]
nierpat[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

nierjan20 <- nierpat[jaar == 2020]

jan20 <- nierpat[jaar ==2020 & maand == 1,
                 .(med_lft = median(lftmaand, na.rm=T),
                   n = .N,
                   n_vrouw = sum(geslacht == 2),
                   perc_vrouw = sum(geslacht == 2) / .N,
                   HD_centrum = sum(therapie == "centrum-HD"),
                   HD_thuis = sum(therapie == "thuis-HD"),
                   PD = sum(therapie == "PD")),
                 by = type]
