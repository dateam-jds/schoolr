# Load packages -----------------------------------------------------------

library(httr)
library(tidyverse)

# Load COVID-19 cases in village level ------------------------------------

resp <- 
  GET("https://covid19-public.digitalservice.id/api/v1/sebaran_v2/jabar")

cases_raw <-
  resp %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data", "content") %>% 
  as_tibble()

cases <-
  cases_raw %>% 
  filter(nchar(kode_kel) == 10) %>% 
  filter(kode_kel != "0000000000") %>% 
  filter(status == "CONFIRMATION") %>% 
  filter(stage == "Diisolasi") %>% 
  group_by(
    district_bps_name = nama_kab,
    village_bps_code = kode_kel,
    latitude,
    longitude
  ) %>% 
    summarise(
      active = n(),
      last_updated = max(as.Date(tanggal_update_nasional))
    ) %>% 
    ungroup() %>% 
    arrange(desc(active))

# Save data into project --------------------------------------------------

usethis::use_data(cases, overwrite = TRUE)
