# Load packages -----------------------------------------------------------

library(tidyverse)

# Load school data --------------------------------------------------------

school <- 
  read_csv(
    "data-raw/school.csv",
    col_types = "_ccccdd",
    skip = 1,
    col_names = c("npsn", "school_name", "district_bps_name", "village_bps_code", "latitude", "longitude")
  )

# Save data into the project ----------------------------------------------

usethis::use_data(school, overwrite = TRUE)
