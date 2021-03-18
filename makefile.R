# Load packages -----------------------------------------------------------

library(tidyverse)
library(geosphere)
library(ggridges)
library(hrbrthemes)

# Load data ---------------------------------------------------------------

update <- FALSE

if (isTRUE(update)) {
  walk(
    list.files("data-raw", full.names = TRUE, pattern = ".\\R$"),
    ~ source(.x, echo = TRUE)
  )
} else {
  walk(
    list.files("data", full.names = TRUE),
    ~ load(.x, envir = .GlobalEnv)
  )
}

# Measure distance between school and active cases within the dist --------

distance_within_district <-
  school %>% 
  select(
    district_bps_name,
    npsn,
    school_lat = latitude,
    school_lon = longitude
  ) %>% 
  left_join(
    cases %>% 
      select(
        district_bps_name,
        village_bps_code,
        village_lat = latitude,
        village_lon = longitude
      ), 
    by = "district_bps_name"
  ) %>% 
  drop_na() %>% 
  rowwise() %>%
  mutate(distance = distVincentyEllipsoid(
    p1 = c(school_lon, school_lat),
    p2 = c(village_lon, village_lat)
  ),
  distance = distance / 1000) %>%
  ungroup()

distance_within_district_plot <- 
  distance_within_district %>% 
  ggplot(aes(distance, district_bps_name)) +
  geom_density_ridges_gradient(aes(fill = after_stat(x)), scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(
    labels = function(x) paste(x, " km"),
    expand = c(0, 0.05)
  ) +
  scale_fill_distiller(palette = "BuPu", guide = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    title = "School distance to neighbouring COVID-19 cases",
    subtitle = "Correspondance within common district area",
    caption = "Analysed by Jabar Digital Service"
  ) +
  theme_ipsum_tw(
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

ggsave(
  "outfile/distance_within_district.png",
  plot = distance_within_district_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

# Combine data ------------------------------------------------------------

alldata <- 
  distance_within_district %>%
  filter(distance <= 10) %>%
  left_join(
    cases %>% 
      select(village_bps_code, active),
    by = "village_bps_code"
  ) %>% 
  group_by(
    npsn
  ) %>% 
  summarise(
    nearest_distance = mean(distance),
    nearest_active = sum(active)
  ) %>% 
  full_join(
    school,
    by = "npsn"
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ replace_na(.x, 0)
    )
  )


# Clustering --------------------------------------------------------------


  



