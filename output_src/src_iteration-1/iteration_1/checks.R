smr_dt <- arrow::read_parquet(
  "data/results/smr_dt_analytical_2016_2023.parquet")
smr_dt <- data.table::as.data.table(smr_dt)

assertthat::assert_that(
  all(smr_dt$total_kidney_deaths ==
        smr_dt$obs_dialyses_deaths + smr_dt$obs_transplant_deaths)
)

smr_dt[, total := total_alive_start + shift(total_deaths, type="lag")]
      

smr_dt[, c("total_alive_start", "total", "total_deaths")] 

##MV: Ik snap niet helemaal waarom total_alive_start + de deaths van vorige maand niet
## gelijk is aan de total_alive_start van de maand ervoor?

smr_dt[, exp_total_alive_start := shift(total_alive_start - total_deaths, type = "lag")]
smr_dt[, diff := total_alive_start - exp_total_alive_start]
smr_dt[, c("total_alive_start", "exp_total_alive_start", "diff", "total_deaths")] 
