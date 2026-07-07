# Fetch kidney dbcs
source("src/setup.R")

kidney_patients <- function(years){
  ## Function to fetch all kidney patient rinpersonen
  #' @param years Years to read
  #' @param n_max Number of lines to read for each file
  #' @return Data.table with kidney patients
  dt <- data.table()
  
  for (yr in years) {
    msz_path <- get_path_newest("G:/GezondheidWelzijn/MSZPRESTATIESVEKTTAB/geconverteerde data",
                                string_pattern = glue::glue("MSZPrestatiesVEKT{yr}"),
                                extension = ".parquet")
    #print(msz_path)
    
    msz_file <- arrow::read_parquet(msz_path, col_select = all_of(fetch_nier_rins))
    setDT(msz_file)
    
    # Save kidney specialisme
    msz_file[, spec := substr(msz_file$VEKTMSZSpecialismeDiagnoseCombinatie, 1, 4)]
    msz_file <- msz_file[spec == "0313"]
    
    # Only keep diagnose codes of interest
    msz_file[, diag := substr(msz_file$VEKTMSZSpecialismeDiagnoseCombinatie, 12, 15)]
    msz_file <- msz_file[diag %in% diags]
    
    # only keep kidney dbcs (This should be redundant)
    msz_file <- msz_file[VEKTMSZDBCZorgproduct %in% dia_trans]
    
    
    #msz_file <- msz_file[, .(RINPERSOON)]
    
    dt <- rbindlist(list(dt, msz_file), use.names = T)
    rm(msz_file)
  }
  return(dt)
}

tictoc::tic("Processing all msz files")
kidney_dt <- kidney_patients(years)
tictoc::toc()

# Only save unique rins
kidney_dt <- kidney_dt[, .(rinpersoon = unique(RINPERSOON))]

#kid <- r_parquet_get_dt("data/nierpatienten/nierpatienten.parquet")

# Write to data folder
setindex(kidney_dt, NULL)
arrow::write_parquet(kidney_dt, "data/nierpatienten/nierpatienten.parquet")
write.csv(kidney_dt, "data/nierpatienten/nierpatienten.csv")
