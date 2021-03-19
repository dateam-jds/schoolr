# Load packages -----------------------------------------------------------

library(tidyverse)

# Load school data --------------------------------------------------------

school <- 
  read_csv(
    "data-raw/school.csv",
    col_types = "ccccddc"
  )

# Save data into the project ----------------------------------------------

usethis::use_data(school, overwrite = TRUE)
