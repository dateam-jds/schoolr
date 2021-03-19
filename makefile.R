# Load packages -----------------------------------------------------------

library(tidyverse)
library(geosphere)
library(ggridges)
library(hrbrthemes)
library(FactoMineR)
library(jabr)
library(sf)

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

basemap <- jabr_basemap(level = "district")

# Our data ----------------------------------------------------------------

cases
school 
village

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

school_nearto_epicentre <- 
  distance_within_district %>%
  filter(distance <= 10) %>%
  left_join(
    cases %>% 
      select(village_bps_code, active),
    by = "village_bps_code"
  ) %>% 
  left_join(
    village,
    by = "village_bps_code"
  ) %>%
  group_by(
    npsn
  ) %>% 
  summarise(
    active = sum(active, na.rm = TRUE),
    sanitation = mean(sanitation == "available", na.rm = TRUE),
    popdens = mean(popdens, na.rm = TRUE)
  ) %>% 
  left_join(
    school %>% 
      select(npsn, internet),
    by = "npsn"
  )
   
school_farfrom_epicentre <- 
  school %>% 
  anti_join(
    school_nearto_epicentre,
    by = "npsn"
  ) %>% 
  left_join(
    village,
    by = "village_bps_code"
  ) %>% 
  transmute(
    npsn,
    active = 0,
    sanitation = if_else(sanitation == "available", 1, 0),
    popdens,
    internet
  )

# Factor Analysis ---------------------------------------------------------

res_famd <-
  bind_rows(
    near = school_nearto_epicentre,
    far = school_farfrom_epicentre,
    .id = "distance"
  ) %>%
  mutate(
    distance = factor(distance),
    internet = factor(
      internet,
      levels = c("poor", "need check", "good", "very good"),
      ordered = TRUE
    )
  ) %>% 
  relocate(distance, .after = internet) %>% 
  column_to_rownames("npsn") %>%
  FAMD(graph = FALSE)

plot.FAMD(res_famd, choix = "ind", lab.ind = FALSE)
plot.FAMD(res_famd, choix = "ind", lab.ind = FALSE, habillage = 4)
plot.FAMD(res_famd, choix = "ind", lab.ind = FALSE, habillage = 5)
plot.FAMD(res_famd, choix = "quanti")
plot.FAMD(res_famd, choix = "quali")
plot.FAMD(res_famd, choix = "var")

# Clustering --------------------------------------------------------------

res_cluster <- 
  HCPC(res_famd, nb.clust = 3, graph = FALSE)

plot.HCPC(res_cluster, choice = "map")
res_cluster$desc.var$quanti.var %>% 
  as_tibble(rownames = "var")
res_cluster$desc.var$test.chi2 %>% 
  as_tibble(rownames = "var")

plot.catdes(res_cluster$desc.var, level = 0.01,
            col.upper = "dodgerblue2",
            col.lower = "salmon")

# Finally -----------------------------------------------------------------

school_cluster <- 
  res_cluster$data.clust %>% 
  rownames_to_column("npsn") %>% 
  right_join(school) %>% 
  mutate(clust = paste("Cluster", clust))


school_cluster_map <- 
  school_cluster %>% 
  ggplot(aes(longitude, latitude, colour = clust)) +
  geom_sf(
    data = basemap,
    inherit.aes = FALSE,
    fill = "#e5e5e3",
    colour = "white",
    size = 0.01
  ) +
  geom_point(alpha = 0.8) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    title = "School clusters in West Java"
  ) +
  theme_ipsum_tw(
    grid = FALSE,
    ticks = TRUE
  ) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "bottom"
  )

ggsave(
  "outfile/school_cluster_map.png",
  plot = school_cluster_map,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

school_cluster_plot <-  
  school_cluster %>% 
  count(
    district_bps_name,
    clust
  ) %>%
  group_by(district_bps_name) %>% 
  mutate(
    pct = n / sum(n)
  ) %>% 
  ungroup() %>% 
  mutate(district_bps_name = fct_reorder2(district_bps_name, clust, pct)) %>% 
  ggplot(aes(pct, district_bps_name, fill = clust)) +
  geom_col() +
  scale_x_percent() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "School clusters proportion within district"
  ) +
  theme_ipsum_tw(
    grid = FALSE,
    ticks = TRUE
  ) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.y = element_text(size = rel(0.8)),
    legend.position = "bottom"
  )

ggsave(
  "outfile/school_cluster.png",
  plot = school_cluster_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)
