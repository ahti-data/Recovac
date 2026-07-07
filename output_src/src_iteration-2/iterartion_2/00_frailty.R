source("src/setup.R")
source("H:/utils/m_functions.R")
library(ggplot2)

kidney <- r_parquet_get_dt("data/raw/nefro_year_month.parquet")
kidney <- kidney[!rinpersoon %in% double_therapie_rins]
kidney20 <- kidney[year == 2020]

dg <- r_parquet_get_dt("H:/data/medischspecialistischezorg_vektis/2020/rin_medischspecialistischezorg_vektis.parquet")
dg46 <- dg[, .(rinpersoon, kosten_DBC_d46_v, kosten_DBC_gem_d46_v)]
dg46[, rinpersoon := as.numeric(rinpersoon)]
dg46ja <- dg46[kosten_DBC_d46_v > 0]


msz <- arrow::read_parquet("G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/geconverteerde data/MSZPrestatiesVEKT2020TABV3.parquet",
                           col_select = all_of(c("VEKTMSZSpecialismeDiagnoseCombinatie", "RINPERSOON", 
                                  "VEKTMSZVergoedbedragZVW", "VEKTMSZVergoedbedragAV",
                                  "VEKTMSZDBCZorgproduct", "VEKTMSZDeclaratiecode")))
setDT(msz)
msz[, spec := substr(msz$VEKTMSZSpecialismeDiagnoseCombinatie, 1, 4)]
msz <- msz[spec %chin% c("0313", "9999")]
msz[, diag := substr(msz$VEKTMSZSpecialismeDiagnoseCombinatie, 12, 15)]
msz <- msz[diag %chin% kosten_diags | VEKTMSZDeclaratiecode %chin% zorgproducten]
msz <- format_data(msz)
msz[, rinpersoon := as.numeric(rinpersoon)]

#msz <- msz[rinpersoon %in% kidney20$rinpersoon]
msz[, vektmszvergoedbedragzvw := as.numeric(vektmszvergoedbedragzvw)]
msz[, vektmszvergoedbedragav := as.numeric(vektmszvergoedbedragav)]
msz[, totaal_bedrag := rowSums(.SD, na.rm=T), .SDcols = c("vektmszvergoedbedragzvw", "vektmszvergoedbedragav")]
msz_pp <- msz[, .(bedrag = sum(totaal_bedrag)), by = rinpersoon]

zkp <- r_parquet_get_dt("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/geconverteerde bestanden/ZVWZORGKOSTEN2020TABV2.parquet")
zkp <- format_data(zkp)
zkp <- zkp[, .(rinpersoon, zvwktotaal, zvwkfarmacie)]

wlz <- haven::read_sav("G:/GezondheidWelzijn/WLZZINTAB/WLZZIN2020TABV1.sav")
wlz <- format_data(wlz)
wlz <- wlz[, .(wlz_zin_cost = sum(bedragwlzzin)), by = rinpersoon]

# use bedragdeclpgb en filter on wetpgb code == 1
wlzgeb <- haven::read_sav("G:/GezondheidWelzijn/PGBWLZWMOJWTAB/PGBWLZWMOJW2020TABV3.sav")
wlzgeb <- format_data(wlzgeb)
wlzgeb <- wlzgeb[wetpgb == "1"]
wlzgeb <- wlzgeb[, .(wlz_pgb_cost = sum(bedragdeclpgb)), by = rinpersoon]


basis <- fread("G:/GezondheidWelzijn/LBZBASISTAB/2020/LBZBASIS2020TABV1.csv")
lbzdiag <- fread("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2020/LBZDIAGNOSEN2020TABV1.csv")

# Combine all costs
cost_dt <- merge(zkp, wlz, by = "rinpersoon", all = T)
cost_dt <- merge(cost_dt, wlzgeb, by = "rinpersoon", all = T)
cost_dt <- merge(cost_dt, msz_pp, all=T)
cost_dt <- merge(cost_dt, dg46ja, by = "rinpersoon", all = T)
cost_dt[is.na(cost_dt)] <- 0

cost_dt[, wlz_total_cost := wlz_zin_cost + wlz_pgb_cost]
cost_dt[, total_cost := wlz_total_cost + zvwktotaal]

cost_dt[, total_zonder_farmacie_nier := total_cost - zvwkfarmacie - bedrag]
cost_dt[, zvwkosten_zonder_farmacie_nier := zvwktotaal - zvwkfarmacie - bedrag]
cost_dt[, zvwkosten_zonder_d46 := zvwktotaal - kosten_DBC_d46_v]
cost_dt[, zvwkosten_zonder_d46_zonder_farm := zvwktotaal - kosten_DBC_d46_v - zvwkfarmacie]
cost_dtkid20 <- cost_dt[rinpersoon %in% kidney20$rinpersoon]
# ZONDER WLZ PROBEREN
kwint <- quantile(cost_dtkid20[, zvwkosten_zonder_farmacie_nier], 
                  probs = seq(0.2, 0.8, 0.2))
kwint

kwint <- quantile(cost_dtkid20[, zvwkosten_zonder_d46_zonder_farm], 
                  probs = seq(0.2, 0.8, 0.2))
kwint

kwint <- quantile(cost_dt[, zvwkosten_zonder_farmacie_nier], 
                  probs = seq(0.2, 0.8, 0.2))
kwint

kwint <- quantile(cost_dt[bedrag > 0, zvwkosten_zonder_d46_zonder_farm], probs = seq(0.2, 0.8, 0.2))
kwint


grens <- quantile(cost_dt$total_cost, 0.99)
cost_dt_order <- cost_dt[total_cost <= grens]
kwint <- quantile(cost_dt_order$total_cost, probs = seq(0.2, 0.8, 0.2))
kwint
deciel <- quantile(cost_dt_order$total_cost, probs = seq(0.1, 0.9, 0.1))
deciel

ggplot(cost_dt_order, aes(x = total_cost)) +
  geom_histogram(bins = 50, fill = "blue") +
  labs(x = "zvw + wlz kosten", y = "aantal mensen") +
  theme_minimal()
