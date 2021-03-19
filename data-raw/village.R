# Load packages -----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

village <- 
  read_csv(
    "data-raw/village.csv",
    col_types = "ccd"
  )

# Save data into the project ----------------------------------------------

usethis::use_data(village, overwrite = TRUE)

