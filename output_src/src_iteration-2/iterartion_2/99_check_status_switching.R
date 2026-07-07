kidney <- r_parquet_get_dt("data/raw/nefro_year_month.parquet")
kidney <- kidney[!rinpersoon %in% double_therapie_rins]
kidney20 <- kidney[year == 2020]
kidney21 <- kidney[year == 2021]

kid20p1 <- kidney20[, .SD, .SDcols = c("rinpersoon", "month3", "month4", "month5", "month6")]
kid20p2 <- kidney20[, .SD, .SDcols = c("rinpersoon", "month6", "month7", "month8", "month9")]
kid20p12 <- kidney20[, .SD, .SDcols = c("rinpersoon", "month3", "month4", "month5", "month6",
                                        "month7", "month8", "month9")]
kid21p1 <- kidney21[, .SD, .SDcols = c("rinpersoon", "month3", "month4", "month5", "month6")]
kid21p2 <- kidney21[, .SD, .SDcols = c("rinpersoon", "month6", "month7", "month8", "month9")]
kid21p12 <- kidney21[, .SD, .SDcols = c("rinpersoon", "month3", "month4", "month5", "month6",
                                        "month7", "month8", "month9")]

kidney20[, n_stat := uniqueN(unlist(.SD)), by = rinpersoon, .SDcols = paste0("month", 1:12)]
kidney20[, .N, by = n_stat]
maand_cols <- paste0("month", 1:12)

switches <- kidney20[, {
  vals <- unlist(.SD)
  from <- vals[-length(vals)]
  to <- vals[-1]
  changed <- from != to
  list(from = from[changed], to = to[changed])
}, by = rinpersoon, .SDcols = maand_cols]

switches[, .N, by = .(from, to)]

kid20p1[, n_stat := uniqueN(unlist(.SD)), by = rinpersoon, .SDcols = paste0("month", 3:6)]
kid20p1[, .N, by = n_stat]

kid20p2[, n_stat := uniqueN(unlist(.SD)), by = rinpersoon, .SDcols = paste0("month", 6:9)]
kid20p2[, .N, by = n_stat]

kid20p12[, n_stat := uniqueN(unlist(.SD)), by = rinpersoon, .SDcols = paste0("month", 3:9)]
kid20p12[, .N, by = n_stat]

kid21p1[, n_stat := uniqueN(unlist(.SD)), by = rinpersoon, .SDcols = paste0("month", 3:6)]
kid21p1[, .N, by = n_stat]

kid21p2[, n_stat := uniqueN(unlist(.SD)), by = rinpersoon, .SDcols = paste0("month", 6:9)]
kid21p2[, .N, by = n_stat]

kid21p12[, n_stat := uniqueN(unlist(.SD)), by = rinpersoon, .SDcols = paste0("month", 3:9)]
kid21p12[, .N, by = n_stat]


