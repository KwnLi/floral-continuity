library(tidyverse)

lc.gf <- read.csv("data/lcareas_gf2021.csv")
lc.hw <- read.csv("data/lcareas_hw2025.csv")
lc.iv <- read.csv("data/lcareas_iverson.csv")

lcflr.gf <- read.csv("data/flowerdens_gf2021_v2.csv")
lcflr.iv <- read.csv("data/flowerdens_iverson.csv")
lcflr.hw <- read.csv("data/flowerdens_hw2025_v2.csv")


#----- Hemberger & Williams -----

flowerbuf.hw <- lc.hw |>
  pivot_longer(cols = CROPLA:RICE, names_to = "raster.name", values_to = "area") |>
  left_join(lcflr.hw, by = "raster.name", relationship = "many-to-many") |>
  filter(!is.na(period_3wk) & !is.na(area)) |>
  mutate(total.flowers = area*flowerdens)

#----- Guezen & Forrest -----

flowerbuf.gf <- lc.gf |>
  pivot_longer(cols = Blackberry:Canola, names_to = "landtype", values_to = "area") |>
  mutate(landtype = gsub("\\.","-", tolower(landtype)) |> replace_values("cucurbita" ~ "squash")) |>
  left_join(lcflr.gf, by = "landtype", relationship = "many-to-many") |>
  filter(!is.na(datePeriod) & !is.na(area)) |>
  mutate(total.flowers = area*flowerdens)

# ----- Iverson -----  too big to store on github

# flowerbuf.iv <- lc.iv |>
#   pivot_longer(cols = Alfalfa:Plums,
#                names_to = "Floral_resources_landcover",
#                values_to = "area") |>
#   mutate(Floral_resources_landcover =
#            replace_values(Floral_resources_landcover,
#                           "Urban.tree" ~ "Urban tree")) |>
#   left_join(lcflr.iv, by = "Floral_resources_landcover") |>
#   filter(!is.na(area)) |>
#   pivot_longer(d1:d365, names_to = "julianDay", values_to = "flowers_m2") |>
#   mutate(
#     total.flowers = area * flowers_m2,
#     julianDay = as.numeric(gsub("d","",julianDay))
#   ) |>
#   mutate(date = as.Date(julianDay, origin=as.Date("2015-12-31"))) |>
#   filter(date >= "2016-03-01" & date <= "2016-09-30")

# ----- write out -----
write.csv(flowerbuf.hw, "data/flowerbuf_hw2025.csv", row.names = FALSE)
write.csv(flowerbuf.gf, "data/flowerbuf_gf2021.csv", row.names = FALSE)
# write.csv(flowerbuf.iv, "data/flowerbuf_iverson.csv", row.names = FALSE)
