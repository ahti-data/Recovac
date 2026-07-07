source("src/setup.R")


nierpat <- fread("data/raw/9096_data9096nefrovisieCBKV1.csv")
nierpat3 <- fread("data/raw/9096_data9096nefrovisieCBKV3.csv")
setDT(nierpat)
setDT(nierpat3)


# nierpat_vars <- c("geboortedatum", "overlijdensdatum", "pcode4", "geslacht", "lftmaand",
#                   "therapie", "jaar", "maand", "RINPersoon")
#nierpat <- nierpat[, ..nierpat_vars]
transplant_therapie <- c( "TX-postmortaal", "TX-levend", "TX-onbekend")
nierpat <- format_data(nierpat)
nierpat <- nierpat[!is.na(rinpersoon)]
nierpat[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

nierpat3 <- format_data(nierpat3)
nierpat3 <- nierpat3[!is.na(rinpersoon)]
nierpat3[, type := fifelse(therapie %chin% transplant_therapie, 2, 1)]

# alle rins die wel in koppeling 3 zitten maar niet in koppeling 1
new_rins <- nierpat3[!rinpersoon %in% nierpat$rinpersoon]

# rins die gedurende de hele periode transplant waren
altijd_ktr <- new_rins[, all(type == 2), by = rinpersoon][V1 == T, rinpersoon]

# data van de altijd ktr mensen
new_ktr <- new_rins[rinpersoon %in% altijd_ktr]
new_ktr[, aanweziginhulpbestand := NULL]

nierpat <- rbindlist(list(nierpat, new_ktr), use.names = T)

# mensen die zowel dialyse als transplant zijn geweest
# beide_types <- nier4[, .(heeft_1 = any(type == 1), heeft_2 = any(type == 2)), by = rinpersoon][
#   heeft_1 == T & heeft_2 == T, rinpersoon
# ]


# Checke duplicates
# dupe <- nierpat[, if (.N > 1) .SD, by = .(rinpersoon, jaar, maand)]
# dupe_diff_therapie <- dupe[, if (uniqueN(therapie) > 1) .SD, by = .(rinpersoon, jaar, maand)]
# 
# setindex(dupe_diff_therapie, NULL)
# arrow::write_parquet(dupe_diff_therapie, "data/raw/kidney_pat_doubles.parquet")

setnames(nierpat, "jaar", "year")
wide <- dcast(unique(nierpat, by = c("rinpersoon", "year", "maand")), 
              rinpersoon + year ~ paste0("month", maand), 
              value.var = "type",
              fill = 0)

setindex(wide, NULL)
arrow::write_parquet(wide, "data/raw/nefro_year_month_k1_k3.parquet")


# NOTE: Include kids here but exclude later in analyses

#' kidney_patients <- function(years){
#'   ## Function to fetch all kidney patient rinpersonen
#'   #' @param years Years to read
#'   #' @param n_max Number of lines to read for each file
#'   #' @return Data.table with kidney patients
#'   dt <- data.table()
#'   
#'   for (yr in years) {
#'     msz_path <- get_path_newest("G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/geconverteerde data",
#'                                 string_pattern = glue::glue("MSZPrestatiesVEKT{yr}"),
#'                                 extension = ".parquet")
#'     
#'     msz <- arrow::read_parquet(msz_path, col_select = all_of(cols_msz_prest))
#'     msz <- format_data(msz)
#'     setnames(msz, 
#'              c("vektmszdbczorgproduct", "vektmszbegindatumprest"), 
#'              c("dbc", "start_datum"))
#'     
#'     
#'     # Keep kidney dbcs
#'     msz <- msz[dbc %in% dia_trans]
#'     
#'     # Keep kidney specialisme
#'     msz[, spec := substr(msz$vektmszspecialismediagnosecombinatie, 1, 4)]
#'     msz <- msz[spec == "0313"]
#'     
#'     # Keep diagnose codes of interest
#'     msz[, diag := substr(msz$vektmszspecialismediagnosecombinatie, 12, 15)]
#'     msz <- msz[diag %in% diags]
#'     
#'     msz[, type_dbc := fifelse(dbc %in% dialyse_codes, 1, 2)]
#'     
#'     
#'     # Define dialyse and transplant
#'     # msz[, dialyse := fifelse(dbc %in% dialyse_codes, 1, 0)]
#'     # msz[, transplant := fifelse(dbc %in% transplant_codes, 1, 0)]
#'     
#'     msz[, year := yr]
#'     
#'     dt <- rbindlist(list(dt, msz), use.names = T)
#'     rm(msz)
#'     
#'   }
#'   return(dt)
#' }
#' 
#' years <- 2016:2023
#' tictoc::tic("Processing all msz files")
#' kidney_dt <- kidney_patients(years)
#' tictoc::toc()
#' 
#' arrow::write_parquet(kidney_dt, "data/raw/patients_dbcs_dates.parquet")
#' kidney_dt <- r_parquet_get_dt("data/raw/patients_dbcs_dates.parquet")
#' 
#' kidney_dt[, start_datum := as.Date(start_datum, format = "%Y%m%d")]
#' 
#' # Extract first time someone becomes kidney patient
#' #dt_start <- kidney_dt[, .(first_start = min(start_datum)), by= rinpersoon]
#' dt_start <- kidney_dt[, .(first_start = min(start_datum)), by= .(rinpersoon, type_dbc)]
#' 
#' # Make wide version for splitting dialyse and transplant
#' dt_wide <- dcast(
#'   dt_start,
#'   rinpersoon ~ type_dbc,
#'   value.var = "first_start"
#' )
#' 
#' setnames(
#'   dt_wide,
#'   old = c("1", "2"),
#'   new = c("start_dialyse", "start_transplant")
#' )
#' 
#' # Make year and month variables long format
#' calender <- dt_wide[, .(#first_start = first_start,
#'                          start_dialyse = start_dialyse,
#'                          start_transplant = start_transplant,
#'                          year = rep(years, each = 12),
#'                          month = rep(1:12, times = length(years))
#' ), by = .(rinpersoon)
#' ]
#' 
#' # Make variable to compare first_start with
#' calender[, month_date := as.Date(paste(year, month, "01", sep="-"))]
#' 
#' # If some month is after the date one has first kidney dbc, you are kidney patient
#' # from that month onwards
#' #calender[, nierpatient := as.integer(month_date > first_start)]
#' calender[, status := 0]
#' 
#' calender[!is.na(start_dialyse) & month_date > start_dialyse, status := 1L]
#' calender[!is.na(start_transplant) & month_date > start_transplant, status := 2L]
#' 
#' 
#' # Each month as column (year as row)
#' dt_final <- dcast(
#'   calender,
#'   rinpersoon + year ~ month,
#'   value.var = "status"
#' )
#' 
#' setnames(
#'   dt_final,
#'   old = as.character(1:12),
#'   new = paste0("month", 1:12) 
#' )
#' 
#' # Count total kidney patients per year/month combination
#' agg <- calender[,
#'                 .(patients = .N),
#'                 by = .(year, month, status)][order(year,month, status)]
#' 
#' 
#' #### Write ####
#' setindex(dt_final, NULL)
#' arrow::write_parquet(dt_final, "data/raw/patient_per_month_and_year.parquet")
#' 
#' setindex(agg, NULL)
#' arrow::write_parquet(agg, "data/raw/patient_count_per_month_and_year.parquet")
