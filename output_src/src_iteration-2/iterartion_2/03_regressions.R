source("src/setup.R")
source("src/functions.R")
library(data.table)
library(openxlsx)
library(fastglm)
library(glue)

ex_immuno <- T

deaths <- r_parquet_get_dt("data/raw/deaths_2020_2021.parquet")
dt <- r_parquet_get_dt("data/raw/regression_data_with_comorb_opname_v2.parquet")

dt[, seswoa_small := fcase(
  seswoa_cat == "0-10%", "0-10%",
  seswoa_cat == "10-20%", "10-20%",
  seswoa_cat == "20-35%", "20-50%",
  seswoa_cat == "35-50%", "20-50%",
  seswoa_cat == "50-75%", "50-100%",
  seswoa_cat == "75-100%", "50-100%",
  seswoa_cat == "Onbekend", "Onbekend"
)]

# Lijsten met rins die we niet mee willen nemen in de regressies
rins_dood_voor_p1_2020 <- deaths[death_year == 2020 & gbadatumoverlijden < as.Date("2020-03-14"), rinpersoon]
rins_dood_voor_p2_2020 <- deaths[death_year == 2020 & gbadatumoverlijden < as.Date("2020-06-14"), rinpersoon]
rins_dood_voor_p1_2021 <- deaths[death_year == 2021 & gbadatumoverlijden < as.Date("2021-03-04"), rinpersoon]
rins_dood_voor_p2_2021 <- deaths[death_year == 2021 & gbadatumoverlijden < as.Date("2021-06-04"), rinpersoon]

#### Model setup ####
dt[, status_maart := factor(status_maart, levels = c(0, 1, 2))]
dt[, status_juni := factor(status_juni, levels = c(0, 1, 2))]
dt[, geslacht := factor(geslacht, levels = c("Vrouwen", "Mannen"))]
dt[, seswoa_small := factor(seswoa_small, 
                            levels = c("50-100%", "20-50%", "10-20%", "0-10%", "Onbekend"))]


dt_2020 <- dt[year == 2020]
dt_2021 <- dt[year == 2021]

# check counts per 
rm(dt)

# Stratified sampling for testing
# set.seed(123)
# dt_2020_sample <- dt_2020[, .SD[sample(.N, min(.N, 1000000))], by = .(status_maart, status_juni)]
# dt_2021_sample <- dt_2021[, .SD[sample(.N, min(.N, 1000000))], by = .(status_maart, status_juni)]

datasets <- list(
  "p1_2020" = list(data = dt_2020[!rinpersoon %in% rins_dood_voor_p1_2020], status = "status_maart"),
  "p2_2020" = list(data = dt_2020[!rinpersoon %in% rins_dood_voor_p2_2020], status = "status_juni"),
  "p1_2021" = list(data = dt_2021[!rinpersoon %in% rins_dood_voor_p1_2021], status = "status_maart"),
  "p2_2021" = list(data = dt_2021[!rinpersoon %in% rins_dood_voor_p2_2021], status = "status_juni")
)

uitkomsten <- c("dood", "covid_dood", "opname", "ic_opname",
                "covid_dood_opname", "positieve_test")

covariate_sets <- list(
  "uni" = c("{status}"),
  "demog" = c("{status}", "leeftijd", "geslacht"),
  "demog_ses" = c("{status}", "leeftijd", "geslacht", "seswoa_small"),
  "demog_comorb_ex_immuno" =
    c("{status}", "leeftijd", "geslacht", "seswoa_small",
      "astma_copd", "diabetes_combi"),
  "demog_comorb" = c("{status}", "leeftijd", "geslacht", "seswoa_small",
                     "immuno", "astma_copd", "diabetes_combi"),
  "demog_opn" = c("{status}", "leeftijd", "geslacht", "seswoa_small",
                           "opname_algemeen"),
  "all_ex_immuno" = c("{status}", "leeftijd", "geslacht", "seswoa_small",
                      "immuno", "astma_copd", "diabetes_combi",
                      "opname_algemeen"),
  "all" = c("{status}", "leeftijd", "geslacht", "seswoa_small", "immuno", 
                  "astma_copd", "diabetes_combi", "opname_algemeen")
)

if (ex_immuno) {
  cov_selected <- names(covariate_sets)[
    grepl("ex_immuno", names(covariate_sets))] 
} else {
  cov_selected <- names(covariate_sets)
}

combinaties <- expand.grid(
  periode = names(datasets),
  uitkomst = uitkomsten,
  covariaten = cov_selected,
  stringsAsFactors = F
)

draai_model <- function(data, uitkomst, covariaten) {
  formule <- as.formula(paste(uitkomst, "~", paste(covariaten, collapse = " + ")))
  X <- model.matrix(formule, data = data)
  y <- data[[uitkomst]]
  
  model <- fastglm(x = X, y = y, family = binomial())
  
  result <- data.table(
    term = names(coef(model)),
    estimate = coef(model),
    std.error = model$se #sqrt(diag(vcov(model)))
  )
  
  result[, statistic := estimate / std.error]
  result[, p.value := 2 * pnorm(abs(statistic), lower.tail = F)]
  result[, log_odds := estimate]
  result[, conf.low := exp(estimate - 1.96 * std.error)]
  result[, conf.high := exp(estimate + 1.96 * std.error)]
  result[, estimate := exp(estimate)]
  
  result[, .(
    term,
    log_odds = round(log_odds, 3),
    OR = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = round(p.value, 5),
    statistic = round(statistic, 3),
    std.error = round(std.error, 3)
  )]
}

run_alle_modellen <- function() {
  res_list <- list()
  
  for (i in 1:nrow(combinaties)) {
    combinatie <- combinaties[i, ]
    periode_info <- datasets[[combinatie$periode]]
    uitkomst_col <- paste0(combinatie$uitkomst, "_", combinatie$periode)
    covariaten_vector <- gsub("\\{status\\}", periode_info$status, covariate_sets[[combinatie$covariaten]])
    
    key <- paste(combinatie$periode, combinatie$uitkomst, combinatie$covariaten,
                 sep = "|")
    res_list[[key]] <- draai_model(periode_info$data, uitkomst_col, covariaten_vector)
  }
  res_list
}

resultaten_ref0 <- run_alle_modellen()

dt_2020[, status_maart := relevel(status_maart, ref = "2")]
dt_2020[, status_juni := relevel(status_juni, ref = "2")]
dt_2021[, status_maart := relevel(status_maart, ref = "2")]
dt_2021[, status_juni := relevel(status_juni, ref = "2")]

datasets <- list(
  "p1_2020" = list(
    data = dt_2020[!rinpersoon %in% rins_dood_voor_p1_2020 & status_maart != 0], 
    status = "status_maart"),
  "p2_2020" = list(
    data = dt_2020[!rinpersoon %in% rins_dood_voor_p2_2020 & status_juni != 0], 
    status = "status_juni"),
  "p1_2021" = list(
    data = dt_2021[!rinpersoon %in% rins_dood_voor_p1_2021 & status_maart != 0], 
    status = "status_maart"),
  "p2_2021" = list(
    data = dt_2021[!rinpersoon %in% rins_dood_voor_p2_2021 & status_juni != 0], 
    status = "status_juni")
)

datasets <- lapply(datasets, function(ds) {
  ds$data <- copy(ds$data)
  ds$data[, (ds$status) := droplevels(get(ds$status))]
  ds
})

rm(dt_2020, dt_2021, deaths)
gc()

resultaten_ref1 <- run_alle_modellen()

# Add degrees of freedom to regressions (code om achteraf toe te voegen dus beetje
# omslachtig)
datasets_ref0 <- list(
  "p1_2020" = list(data = dt_2020[!rinpersoon %in% rins_dood_voor_p1_2020], status = "status_maart"),
  "p2_2020" = list(data = dt_2020[!rinpersoon %in% rins_dood_voor_p2_2020], status = "status_juni"),
  "p1_2021" = list(data = dt_2021[!rinpersoon %in% rins_dood_voor_p1_2021], status = "status_maart"),
  "p2_2021" = list(data = dt_2021[!rinpersoon %in% rins_dood_voor_p2_2021], status = "status_juni")
)

df_residuals_ref0 <- numeric(length(names(resultaten_ref0)))
names(df_residuals_ref0) <- names(resultaten_ref0)

for (key in names(resultaten_ref0)) {
  delen <- strsplit(key, "\\|")[[1]]
  periode <- delen[1] 
  n_obs <- nrow(datasets_ref0[[periode]]$data)
  n_params <- nrow(resultaten_ref0[[key]])
  df_residuals_ref0[key] <- n_obs - n_params
}

df_residuals_ref1 <- numeric(length(names(resultaten_ref1)))
names(df_residuals_ref1) <- names(resultaten_ref1)
# Update datasets for kidney patient regressions DF
datasets_ref1 <- list(
  "p1_2020" = list(
    data = dt_2020[!rinpersoon %in% rins_dood_voor_p1_2020 & status_maart != 0], 
    status = "status_maart"),
  "p2_2020" = list(
    data = dt_2020[!rinpersoon %in% rins_dood_voor_p2_2020 & status_juni != 0], 
    status = "status_juni"),
  "p1_2021" = list(
    data = dt_2021[!rinpersoon %in% rins_dood_voor_p1_2021 & status_maart != 0], 
    status = "status_maart"),
  "p2_2021" = list(
    data = dt_2021[!rinpersoon %in% rins_dood_voor_p2_2021 & status_juni != 0], 
    status = "status_juni")
)

for (key in names(resultaten_ref1)) {
  delen <- strsplit(key, "\\|")[[1]]
  periode <- delen[1] 
  n_obs <- nrow(datasets_ref1[[periode]]$data)
  n_params <- nrow(resultaten_ref1[[key]])
  df_residuals_ref1[key] <- n_obs - n_params
}


# Combine and save data

workbooks <- list()
for (periode in names(datasets)) {
  workbooks[[periode]] <- createWorkbook()
}

for (key in names(resultaten_ref0)) {
  
  delen <- strsplit(key, "\\|")[[1]]
  periode <- delen[1]
  uitkomst <- delen[2]
  cov_set <- delen[3]
  tab_naam <- paste0(uitkomst, "_", cov_set)
  
  df_ref0 <- df_residuals_ref0[[key]]
  df_ref1 <- df_residuals_ref1[[key]]
  
  if (df_ref0 < 10 || df_ref1 < 10){
    cat(period, uitkomst)
  }
  
  dt_ref0 <- copy(resultaten_ref0[[key]])
  dt_ref1 <- copy(resultaten_ref1[[key]])
  
  # Add referentie category of nierpatient status 
  dt_ref0[term == "status_maart1" | term == "status_juni1", 
          referentie := "ref = 0 (geen nierpatient)"]
  dt_ref1[term == "status_maart1" | term == "status_juni1", 
          referentie := "ref = 2 (transplant)"]
  
  setcolorder(dt_ref0, c("referentie", setdiff(names(dt_ref0), "referentie")))
  setcolorder(dt_ref1, c("referentie", setdiff(names(dt_ref1), "referentie")))
  
  dt_ref0[, df_residual := NA_integer_]
  dt_ref0[1, df_residual := df_ref0]
  
  dt_ref1[, df_residual := NA_integer_]
  dt_ref1[1, df_residual := df_ref1]
  
  
  lege_rij <- data.table(referentie = c(NA_character_, NA_character_))
  
  dt_combined <- rbind(dt_ref0, lege_rij, dt_ref1, fill = T)
  
  addWorksheet(workbooks[[periode]], sheetName = substr(tab_naam, 1, 30))
  writeData(workbooks[[periode]], sheet = substr(tab_naam, 1, 30), dt_combined)
}


# Modellen draaien en wegschrijven
# for (i in 1:nrow(combinaties)) {
#   combinatie <- combinaties[i, ]
#   periode_info <- datasets[[combinatie$periode]]
#   uitkomst_col <- paste0(combinatie$uitkomst, "_", combinatie$periode)
#   covariaten_vector <- gsub("\\{status\\}", periode_info$status, covariate_sets[[combinatie$covariaten]])
#   
#   dt_tab <- draai_model(periode_info$data, uitkomst_col, covariaten_vector)
#   
#   tab_naam <- paste0(combinatie$uitkomst, "_", combinatie$covariaten)
#   
#   addWorksheet(workbooks[[combinatie$periode]], sheetName = tab_naam)
#   writeData(workbooks[[combinatie$periode]], sheet = tab_naam, dt_tab)
# }

for (periode in names(workbooks)) {
  saveWorkbook(
    workbooks[[periode]],
    file = paste0(
      "data/results/regressies/", periode,
      glue("{ifelse(ex_immuno, 'ex_immuno', '')}_final_met_df.xlsx")),
    overwrite = T)
}




# glm en fastglm zelfde results
# data_alt <- copy(dt_2020_sample)
# data_alt[, status_maart := relevel(status_maart, ref = "1")]
# 
# formule <- as.formula("dood_p1_2020 ~ status_maart + leeftijd + geslacht + seswoa_cat")
# 
# # normal glm
# glm_ref0 <- glm(formule, data = dt_2020_sample[!rinpersoon %in% rins_dood_voor_p1_2020],
#                 family = binomial)
# print(summary(glm_ref0))
# 
# # normal glm ref1
# glm_ref1 <- glm(formule, data= data_alt, family = binomial)
# print(summary(glm_ref1))
# 
# 
# # 
# # fastglm, ref0
# X_r0 <- model.matrix(formule, data= dt_2020_sample)
# y <- dt_2020_sample[["dood_p1_2020"]]
# fglm_r0 <- fastglm(x = X_r0, y = y, family = binomial())
# print(data.frame(
#   coef = coef(fglm_r0),
#   se = fglm_r0$se
# ))
# 
# # fastglm, ref1
# X_r1 <- model.matrix(formule, data= data_alt)
# fglm_r1 <- fastglm(x = X_r1, y = y, family = binomial())
# print(data.frame(
#   coef = coef(fglm_r1),
#   se = fglm_r1$se
# ))
# 
# coef1 <- coef(fglm_ref0)["status_maart1"]
# coef2 <- coef(fglm_ref0)["status_maart2"]
# var1 <- vcov_matrix["status_maart1", "status_maart1"]
#1.028810 - 1.835324 






#dt[, status_maart := relevel(status_maart, ref = "1")]

# covariaten <- c("status_maart", "leeftijd", "geslacht", "seswoa_cat")
# 
# p1_2020 <- dt[year == 2020 & !rinpersoon %in% rins_dood_voor_p1_2020]
# 
# output <- draai_model(p1_2020, "dood_p1_2020", covariaten, "p1 2020")
# 
# formule <- as.formula(paste("dood_p1_2020", "~", paste(covariaten, collapse = " + ")))
# model1 <- glm(formule, data = p1_2020, family = binomial)
# 


