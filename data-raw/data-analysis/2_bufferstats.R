library(tidyverse)

flowerbuf.gf <- read.csv("data/flowerbuf_gf2021.csv")
flowerbuf.hw <- read.csv("data/flowerbuf_hw2025.csv")
lc.iv <- read.csv("data/lcareas_iverson.csv")
lcflr.iv <- read.csv("data/flowerdens_iverson.csv")
#
# flr.gf <- read.csv("data/florlc_gf2021.csv")
# flr.hw <- read.csv("data/florlc_hw2025.csv")
#
# lcflr.gf <- read.csv("data/flowerdens_gf2021_v2.csv")
# lcflr.iv <- read.csv("data/flowerdens_iverson.csv")
# lcflr.hw <- read.csv("data/flowerdens_hw2025_v2.csv")
#
# key.iv <- read.csv("data-raw/Iversonetal/iverson_landcover_codes.csv")
# lckey.hw <- read.csv("data/spatial/hw2025_lc_key.csv")

# ---- Hemberger and Williams data ----

buf.hw <- flowerbuf.hw |> filter(buffer == 750)

croparea.hw <- buf.hw |>
  group_by(period_3wk, SITE, landcover.4cat) |>
  summarize(area = sum(area), total.flowers = sum(total.flowers), .groups = "drop") |>
  pivot_wider(id_cols = c(period_3wk, SITE),
              names_from = landcover.4cat, values_from = c(area, total.flowers)) |>
  mutate(
    total.flowers = total.flowers_crop + total.flowers_natural,
    total.area = area_crop + area_natural,
    pct.natural = 100*area_natural/(750*750*pi),
    pct.nat.intv = cut_width(pct.natural, width = 25, boundary = 0)
  )

croprich.hw <- buf.hw |> rowwise() |>
  mutate(crop = landcover.4cat=="crop" & area > 0) |>
  group_by(period_3wk, SITE) |>
  summarize(total.flowers = sum(total.flowers), crop.rich = sum(crop),
            .groups = "drop")

croparearich.hw <- croparea.hw |> select(period_3wk, SITE, area_crop, area_natural, pct.natural, total.flowers) |>
  left_join(croprich.hw |> select(period_3wk, SITE, crop.rich), by = c("period_3wk", "SITE"))

cropdiv.hw <- croparea.hw |> select(SITE, area_crop) |> distinct() |>
  right_join(buf.hw, by = "SITE") |> filter(landcover.4cat == "crop") |>
  mutate(P_i = area/area_crop, P_i2 = P_i * log(P_i)) |>
  group_by(period_3wk, SITE) |>
  summarize(crop.shandiv = - sum(P_i2, na.rm = TRUE), .groups = "drop") |>
  right_join(croparearich.hw, by = c("period_3wk", "SITE"))

# ---- Guezen and Forrest 2021 ----

buf.gf <- flowerbuf.gf |> filter(buffer == 750)

croparea.gf <- buf.gf |>
  group_by(datePeriod, transect, crop) |>
  summarize(area = sum(area), total.flowers = sum(total.flowers), .groups = "drop") |>
  pivot_wider(id_cols = c(datePeriod, transect),
              names_from = crop, values_from = c(area, total.flowers)) |>
  mutate(
    total.flowers = total.flowers_crop + total.flowers_natural,
    total.area = area_crop + area_natural,
    pct.natural = 100*area_natural/(750*750*pi),
    pct.nat.intv = cut_width(pct.natural, width = 25, boundary = 0)
  )

croprich.gf <- buf.gf |> rowwise() |>
  mutate(crop = crop=="crop" & area > 0) |>
  group_by(datePeriod, transect) |>
  summarize(total.flowers = sum(total.flowers), crop.rich = sum(crop),
            .groups = "drop")

croparearich.gf <- croparea.gf |> select(datePeriod, transect, area_crop, area_natural, pct.natural, total.flowers) |>
  left_join(croprich.gf |> select(datePeriod, transect, crop.rich), by = c("datePeriod", "transect"))

cropdiv.gf <- croparea.gf |> select(transect, area_crop) |> distinct() |>
  right_join(buf.gf, by = "transect") |> filter(crop == "crop") |>
  mutate(P_i = area/area_crop, P_i2 = P_i * log(P_i)) |>
  group_by(datePeriod, transect) |>
  summarize(crop.shandiv = - sum(P_i2, na.rm = TRUE), .groups = "drop") |>
  right_join(croparearich.gf, by = c("datePeriod", "transect"))

# ---- Iverson et al. ----

buf.iv <- lc.iv |> filter(buffer == "buf750") |>
  pivot_longer(cols = Alfalfa:Plums,
               names_to = "Floral_resources_landcover",
               values_to = "area") |>
  mutate(Floral_resources_landcover =
           replace_values(Floral_resources_landcover,
                          "Urban.tree" ~ "Urban tree")) |>
  left_join(lcflr.iv, by = "Floral_resources_landcover") |>
  filter(!is.na(area)) |>
  pivot_longer(d1:d365, names_to = "julianDay", values_to = "flowers_m2") |>
  mutate(
    total.flowers = area * flowers_m2,
    julianDay = as.numeric(gsub("d","",julianDay))
  ) |>
  mutate(date = as.Date(julianDay, origin=as.Date("2015-12-31"))) |>
  filter(date >= "2016-03-01" & date <= "2016-09-30")


croparea.iv <- buf.iv |>
  group_by(site, Group, date) |>
  summarize(area = sum(area), total.flowers = sum(total.flowers), .groups = "drop") |>
  pivot_wider(id_cols = c(date, site),
              names_from = Group, values_from = c(area, total.flowers)) |>
  mutate(
    total.flowers = rowSums(across(total.flowers_Agriculture:total.flowers_Wetland)),
    total.area = rowSums(across(area_Agriculture:area_Wetland)),
    area_natural = area_Forest + area_Wetland + area_Succesional + area_Ditch,
    pct.natural = 100*area_natural/(750*750*pi),
    pct.nat.intv = cut_width(pct.natural, width = 25, boundary = 0)
  )

croprich.iv <- buf.iv |> rowwise() |>
  mutate(crop = Group=="Agriculture" & area > 0) |>
  group_by(date, site) |>
  summarize(total.flowers = sum(total.flowers), crop.rich = sum(crop), .groups = "drop")

croparearich.iv <- croparea.iv |> select(date, site, area_Agriculture, area_natural, pct.natural, total.flowers) |>
  rename(area_crop = area_Agriculture) |>
  left_join(croprich.iv |> select(date, site, crop.rich), by = c("date", "site"))

cropdiv.iv <- croparea.iv |> select(site, area_Agriculture) |> distinct() |>
  right_join(buf.iv, by = "site") |> filter(Group == "Agriculture") |>
  mutate(P_i = area/area_Agriculture, P_i2 = P_i * log(P_i)) |>
  group_by(date, site) |>
  summarize(crop.shandiv = - sum(P_i2, na.rm = TRUE), .groups = "drop") |>
  right_join(croparearich.iv, by = c("date", "site"))

# Aaron's data sampled at 5 timepoints

buf.iv_5 <-lc.iv |> filter(buffer == "buf750") |>
  pivot_longer(cols = Alfalfa:Plums,
               names_to = "Floral_resources_landcover",
               values_to = "area") |>
  mutate(Floral_resources_landcover =
           replace_values(Floral_resources_landcover,
                          "Urban.tree" ~ "Urban tree")) |>
  left_join(lcflr.iv, by = "Floral_resources_landcover") |>
  filter(!is.na(area)) |>
  pivot_longer(d1:d365, names_to = "julianDay", values_to = "flowers_m2") |>
  mutate(
    total.flowers = area * flowers_m2,
    julianDay = as.numeric(gsub("d","",julianDay))
  ) |>
  mutate(date = as.Date(julianDay, origin=as.Date("2015-12-31"))) |>
  filter(date %in% c("2016-04-15","2016-05-15","2016-06-15","2016-07-15","2016-08-15"))


croparea.iv_5 <- buf.iv_5 |>
  group_by(site, Group, date) |>
  summarize(area = sum(area), total.flowers = sum(total.flowers), .groups = "drop") |>
  pivot_wider(id_cols = c(date, site),
              names_from = Group, values_from = c(area, total.flowers)) |>
  mutate(
    total.flowers = rowSums(across(total.flowers_Agriculture:total.flowers_Wetland)),
    total.area = rowSums(across(area_Agriculture:area_Wetland)),
    area_natural = area_Forest + area_Wetland + area_Succesional,
    pct.natural = 100*area_natural/(750*750*pi),
    pct.nat.intv = cut_width(pct.natural, width = 25, boundary = 0)
  )

croprich.iv_5 <- buf.iv_5 |> rowwise() |>
  mutate(crop = Group=="Agriculture" & area > 0) |>
  group_by(date, site) |>
  summarize(total.flowers = sum(total.flowers), crop.rich = sum(crop), .groups = "drop")

croparearich.iv_5 <- croparea.iv_5 |> select(date, site, area_Agriculture, area_natural, pct.natural, total.flowers) |>
  rename(area_crop = area_Agriculture) |>
  left_join(croprich.iv |> select(date, site, crop.rich), by = c("date", "site"))

cropdiv.iv_5 <- croparea.iv_5 |> select(site, area_Agriculture) |> distinct() |>
  right_join(buf.iv, by = "site") |> filter(Group == "Agriculture") |>
  mutate(P_i = area/area_Agriculture, P_i2 = P_i * log(P_i)) |>
  group_by(date, site) |>
  summarize(crop.shandiv = - sum(P_i2, na.rm = TRUE), .groups = "drop") |>
  right_join(croparearich.iv_5, by = c("date", "site"))

# ---- write out data ----

write.csv(cropdiv.gf, "data/bufferstats_gf2021.csv", row.names = FALSE)
write.csv(cropdiv.hw, "data/bufferstats_hw2025.csv", row.names = FALSE)
write.csv(cropdiv.iv, "data/bufferstats_iverson.csv", row.names = FALSE)
write.csv(cropdiv.iv_5, "data/bufferstats5_iverson.csv", row.names = FALSE)

saveRDS(cropdiv.iv, "data/bufferstats_iverson.rds")
saveRDS(cropdiv.iv_5, "data/bufferstats5_iverson.rds")
