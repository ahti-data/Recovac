# setup
source("H:/utils/m_functions.R")
source("H:/utils/demog_functions.R")
library(data.table)

years <- 2016:2023

stapeling_cols <- c("leeftijd", "geslacht", "huishsamstsocec", "belanginkbronhh",
                    "belanginkbronhh", "percsm", "gem")

fetch_nier_rins <- c("RINPERSOON", "VEKTMSZDBCZorgproduct", 
                     "VEKTMSZSpecialismeDiagnoseCombinatie")

cols_msz_prest = c(
  "RINPERSOON", "VEKTMSZDBCZorgproduct","VEKTMSZSpecialismeDiagnoseCombinatie",
  'VEKTMSZBegindatumPrest')

dialyse_codes <- c(140301003, 140301006, 140301022, 140301007, 140301023, 140301008, 
             140301024, 140301041, 140301043, 990003004, 140301017, 140301018,
             979002345, 140301019, 140301020)

transplant_codes <- c(979002321, 979002362, 979002283, 979002358, 979002280, 979002319,
                979002288, 979002361, 979002300, 979002303, 979002360, 979002313,
                979002314, 979002316, 979002357, 979002285, 979002287, 979002356,
                979002276, 979002341, 979002282, 979002286, 979002299, 979002311,
                979002279, 979002302, 979002309, 979002328, 979002275)

# all codes combined for easier use
dia_trans <- c(140301003, 140301006, 140301022, 140301007, 140301023, 140301008, 
               140301024, 140301041, 140301043, 990003004, 140301017, 140301018,
               979002345, 140301019, 140301020, 979002321, 979002362, 979002283, 
               979002358, 979002280, 979002319, 979002288, 979002361, 979002300, 
               979002303, 979002360, 979002313, 979002314, 979002316, 979002357, 
               979002285, 979002287, 979002356, 979002276, 979002341, 979002282, 
               979002286, 979002299, 979002311, 979002279, 979002302, 979002309, 
               979002328, 979002275)
assertthat::assert_that(length(dialyse_codes) + length(transplant_codes) == length(dia_trans))


diags <- c("0336", "0339", "0332", "0331", "0076")

spec <- "0313"


# covid uccodes
covid_code <- "U071" 
presumed_covid_code <- "U072" 


# LBZBASISTAB

lbz_vars <- c("RINPERSOON", "LBZIcd10hoofddiagnose", "LBZOpnamedatum", 
              "LBZICopnamedag", "LBZICaantaldagen")

