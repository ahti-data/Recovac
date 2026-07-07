source("src/setup.R")
years <- 2016:2023

# Covid test file
covid_tests <- r_parquet_get_dt("data/raw/covid_tests.parquet")
covid_tests[, c("typecovid19test", "typeuitslagcovid19test") := NULL]

# Prepare death date file
death_file <- "G:/Bevolking/GBAOVERLIJDENTAB/2023/GBAOVERLIJDEN2023TABV1.csv"
deaths <- fread(death_file, select = c("RINPERSOON", "GBADatumOverlijden"))
deaths <- deaths[GBADatumOverlijden >= 20160101 & GBADatumOverlijden < 20240101]
deaths <- format_data(deaths)

# Combine death causes per year to 1 dt
combine_yearly_death_causes <- function(years) {
  
  dt <- data.table()
  for (yr in years) {
    path <- get_path_newest(
      file.path("G:/GezondheidWelzijn/DOODOORZTAB", 
                yr), 
      string_pattern=yr,
      extension=".csv")
    
    death_causes_dt <- fread(path, select = c("RINPERSOON", "UCCODE"))
    
    death_causes_dt <- format_data(death_causes_dt)
    
    
    dt <- rbindlist(list(dt, death_causes_dt), use.names = T)
  
  }
  return(dt)
}

death_causes <- combine_yearly_death_causes(2016:2023)

# Merge causes with dates. Keep only rows present in both data tables
# because deaths contains deaths of Dutch people outside Netherlands and death_causes
# contains a few deaths from 2015 which are placed in 2016.
deaths <- merge(deaths, death_causes, all=F)

# There are 3 people with 2 death causes (all non-covid) so just take 1
deaths <- deaths[, .SD[1], by=rinpersoon]

# Wrangle dates
deaths[, gbadatumoverlijden := as.Date(as.character(gbadatumoverlijden), format = "%Y%m%d")]
deaths[, death_year := year(gbadatumoverlijden)]
deaths[, death_month := month(gbadatumoverlijden)]

# Set NA values to onbekend
#deaths[is.na(uccode), uccode := "onbekend"]

# covid death var
# NOTE: be aware death cause unknown also gets value 0 here
deaths[, covid_death := fifelse(uccode == covid_code,1,0)]
deaths[, presumed_covid_death := fifelse(uccode == presumed_covid_code,1,0)]

# Add covid test week before death as death cause
##MV: dit is een soort temporary merge? moeilijk om te overzien voor mij
## hoe dit werk want als je meerdere tests hebt dan maakt ie dus temporarily
## meer rijen in de post-merge en die worden daarna weer collapsed?
## misschien goed om eerst te mergen dan die nieuwe var te maken, maar
## ik geloof je ook als je het gecheckt hebt.
deaths[covid_tests,
       covid_test_death := fifelse(covid_death ==0 & presumed_covid_death == 0 &
                                     datum_besmetting >= gbadatumoverlijden - 7 &
                                     datum_besmetting <= gbadatumoverlijden, 1,0
       ),
       on = "rinpersoon",
       by = .EACHI
]
deaths[is.na(covid_test_death), covid_test_death := 0]

# Remove NA's or not? 
# -> removing can create bias, you dont know the group that you are throwing away
# can be that covid is better monitored than other things so you are only deleting
# non covid deaths.
# Setting them to onbekend en covid deaths to 0 for onbekend is maybe also incorrect

##MV: Ik zou idd gewoon groep onbekend maken, ook al is dat niet per se at random

# Make year and month variables long format
calender <- deaths[, .(datum_overlijden = gbadatumoverlijden,
                       covid_death, 
                       presumed_covid_death,
                       covid_test_death,
                       death_year,
                       death_month,
                       year = rep(years, each = 12),
                       month = rep(1:12, times = length(years))
), by = rinpersoon
]

calender[, status := 0L]

# Update death status of people who died that month
calender[year == death_year & month == death_month,
         status := fcase(
           covid_death == 1, 2L,
           presumed_covid_death == 1, 3L,
           covid_test_death ==1, 4L,
           default = 1L
         )
]
# Update death status of people who have previously died
calender[year > death_year | (year == death_year & month > death_month),
         status := fcase(
           covid_death == 1, -2L,
           presumed_covid_death == 1, -3L,
           covid_test_death ==1, -4L,
           default = -1L
         )
]

# Each month as column (year as row)
dt_wide <- dcast(
  calender,
  rinpersoon + year ~ month,
  value.var = "status"
)

setnames(
  dt_wide,
  old = as.character(1:12),
  new = paste0("month", 1:12) 
)

# Count total deaths per year/month combination
agg <- calender[status %in% c(1L, 2L, 3L, 4L),
                .(deaths_total = .N,
                  deaths_non_covid = sum(status == 1L),
                  deaths_covid = sum(status == 2L),
                  deaths_covid_presumed = sum(status == 3L),
                  deaths_covid_test = sum(status == 4L)
                  ),
                by = .(year, month)][order(year,month)]


#### checks ####
length(setdiff(deaths$rinpersoon, death_causes$rinpersoon))
# -> we dont have everyones death cause. Death causes are not placed in the wrong year
# because if we take all death causes from 2013-2024, still same amount of death causes missing

length(setdiff(death_causes$rinpersoon, deaths$rinpersoon))
# 79 people are in death causes but not in death dates. This can be solved by
# taking wider death dates, so we assume these people are wrongfully placed
# between 2016-2023 (we assume death dates are leading)


#### Write to data folder ####
setindex(dt_wide, NULL)
arrow::write_parquet(dt_wide, "data/raw/death_per_month_year.parquet")

setindex(agg, NULL)
arrow::write_parquet(agg, "data/raw/death_count_per_month_year.parquet")


