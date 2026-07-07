# Make output
source("H:/utils/m_functions.R")
library(data.table)
library(glue)

file_version <- "dia_ktr_cvd_cancer_samen.xlsx"  ## "_v2.parquet
result_folder <- "data/output/recovac output 19062026/" ## "data/results/"

get_file <- function(path, xlsx=T) {
  if (xlsx) {
    as.data.table(readxl::read_xlsx(path))
  } else {
    r_parquet_get_dt(path)
  }
}

dt_both <- get_file(
  glue("{result_folder}/smr_dt_bootstrap_both_final{file_version}"),
  xlsx = grepl("xlsx", file_version))


keep_vars <- as.character(names(dt_both))
keep_vars <- keep_vars[!grepl("mort_ratio", keep_vars)]

# keep_vars <- c("year", "month", "obs_dialyses_deaths_allcause", "smr_obs_dialyses_allcause", 
#                "ci_low_dialyse_allcause", "ci_high_dialyse_allcause", 
#                "obs_dialyses_deaths_covid","smr_obs_dialyses_covid",       
#                "ci_low_dialyse_covid", "ci_high_dialyse_covid", "dialyse_alive",    
#                "mort_ratio_dialyse_allcause", "mort_ratio_dialyse_covid", "wave")

dt_both <- get_file(
  glue("{result_folder}/smr_dt_bootstrap_both_final{file_version}"),
  xlsx = grepl("xlsx", file_version))[
  , ..keep_vars
]

dt_exp <- get_file(
  glue("{result_folder}/smr_dt_bootstrap_exp_final{file_version}"),
  xlsx = grepl("xlsx", file_version))[
  , ..keep_vars
]

dt_obs <- get_file(
  glue("{result_folder}/smr_dt_bootstrap_obs_final{file_version}"),
  xlsx = grepl("xlsx", file_version))[
    , ..keep_vars
  ]

not_measure <- keep_vars[
  grepl("^obs|year|month|^total_|_alive$|mort_|date|wave|_deaths$", keep_vars)]

measure_vars <- keep_vars[!(keep_vars %in% not_measure)]
 
group_vars <- c(measure_vars, "year", "month")

dt_exp <- dt_exp[, ..group_vars]
dt_obs <- dt_obs[, ..group_vars]

setnames(dt_both, measure_vars,
         paste0(measure_vars, "_O_E"))
setnames(dt_exp, measure_vars,
         paste0(measure_vars, "_E"))
setnames(dt_obs, measure_vars,
         paste0(measure_vars, "_O"))


output_dt <- merge(dt_both, dt_exp, all.x=T)
output_dt <- merge(output_dt, dt_obs, all.x=T)

dialyse_allcause_cols <- grep("dialyse_allcause", names(output_dt), value =T)
dialyse_covid_cols <- grep("dialyse_covid", names(output_dt), value = T)
dialyse_cancer_cols <- grep("dialyse_cancer", names(output_dt), value = T)

transplant_allcause_cols <- grep(
  "transplant_allcause", names(output_dt), value =T)
transplant_covid_cols <- grep("transplant_covid", names(output_dt), value = T)
transplant_cancer_cols <- grep("transplant_cancer", names(output_dt), value = T)

output_dt[obs_dialyse_allcause < 10, (dialyse_allcause_cols) := NA]
output_dt[obs_dialyse_covid < 10, (dialyse_covid_cols) := NA]
output_dt[obs_dialyse_cancer_cvd < 10, (dialyse_covid_cols) := NA]

output_dt[obs_transplant_allcause < 10, (transplant_allcause_cols) := NA]
output_dt[obs_transplant_covid < 10, (transplant_covid_cols) := NA]
output_dt[obs_dialyse_cancer_cvd < 10, (transplant_cancer_cols) := NA]

not_measure

round_cols <- c("total_alive_start", "total_deaths", "total_covid_deaths",
                "total_kidney_deaths", "kidney_covid_deaths",
                "dialyse_alive", "transplant_alive",
                "obs_dialyse_allcause", "obs_dialyse_covid",
                "obs_dialyse_cancer_cvd",
                "obs_transplant_allcause", "obs_transplant_covid",
                "obs_transplant_cancer_cvd")


output_dt[, (round_cols) := lapply(.SD, function(x) round (x/5) * 5), .SDcols = round_cols]

dialyse_allcause_cols <- names(output_dt)[grepl("dialyse_allcause",
                                                names(output_dt))]
dialyse_covid_cols <- names(output_dt)[grepl("dialyse_covid",
                                                names(output_dt))]
dialyse_cancer_cvd_cols <- names(output_dt)[grepl("dialyse_cancer_cvd",
                                                names(output_dt))]
transplant_allcause_cols <- names(output_dt)[grepl("transplant_allcause",
                                                names(output_dt))]
transplant_covid_cols <- names(output_dt)[grepl("transplant_covid",
                                             names(output_dt))]
transplant_cancer_cvd_cols <- names(output_dt)[grepl("transplant_cancer_cvd",
                                                  names(output_dt))]

non_obs_round_cols <- round_cols[!grepl("^obs_", round_cols)]

order_cols <- c("year", "month", "date", "wave", non_obs_round_cols,
                dialyse_allcause_cols, dialyse_covid_cols,
                dialyse_cancer_cvd_cols, transplant_allcause_cols,
                transplant_covid_cols, transplant_cancer_cvd_cols)

for (rc in round_cols) {
  output_dt[[rc]][output_dt[[rc]] == 0] <- NA
}


setindex(output_dt, NULL)
# arrow::write_parquet(output_dt, "data/output/260622_RECOVAC/smr_data.xlsx")
fwrite(output_dt[, ..order_cols]
, "data/output/260622_RECOVAC/smr_data_output.xlsx")

##
data <- readODS::read_ods(
  "data/output/recovac output 19062026/flow diagram ktr.ods")

data$aantal <- DescTools::RoundTo(data$aantal, 5)
data$excluded <- DescTools::RoundTo(data$excluded, 5)

fwrite(data,
       "data/output/260622_RECOVAC/flow_diagram_ktr.xlsx"
       )

##
sheets <- readxl::excel_sheets(
  "data/output/recovac output 19062026/descriptives_iteratie2.xlsx"
)

sheet_data <- lapply(sheets, function(s) {
  temp <- readxl::read_xlsx(
    "data/output/recovac output 19062026/descriptives_iteratie2.xlsx",
    sheet=s
  )
  if ("n" %in% names(temp)) {
    temp$n <- DescTools::RoundTo(temp$n, 5)
    temp <- temp[temp$n > 10, ]
  }
  
  return(temp)
})


names(sheet_data) <- sheets

openxlsx::write.xlsx(
  sheet_data,
  "data/output/260622_RECOVAC/descriptives.xlsx"
)
