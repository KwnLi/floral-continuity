library(tidyverse)
library(marginaleffects)
library(sf)

# ----- Calculate stats on land cover types -----

lcflr.gf <- read.csv("data/flowerdens_gf2021_v2.csv")

lcflr.iv <- read.csv("data/flowerdens_iverson.csv") |>
  pivot_longer(d1:d365, names_to = "julianDay", values_to = "flowers_m2") |>
  mutate(
    julianDay = as.numeric(gsub("d","",julianDay))
  ) |>
  mutate(date = as.character(as.Date(julianDay, origin=as.Date("2015-12-31")))) |>
  filter(date %in% c("2016-04-15","2016-05-15","2016-06-15","2016-07-15","2016-08-15")) |>
  mutate(
    datePeriod =
      recode_values(date,
                    "2016-04-15"~1,
                    "2016-05-15"~2,
                    "2016-06-15"~3,
                    "2016-07-15"~4,
                    "2016-08-15"~5)) |>
  mutate(crop = recode_values(Group,
                              c("Agriculture") ~ "crop",
                              c("Forest","Ditch","Wetland","Successional") ~ "natural",
                              default = "other"
  )) |> rename(landtype = Floral_resources_landcover, flowerdens = flowers_m2)

lcflr.hw <- read.csv("data/flowerdens_hw2025_v2.csv") |>
  rename(crop=landcover.4cat, landtype=landcover.floral, datePeriod=period_3wk)

lcflr <- list(gf=lcflr.gf |> select(landtype, crop, flowerdens, datePeriod),
              iv=lcflr.iv |> select(landtype, crop, flowerdens, datePeriod),
              hw=lcflr.hw |> select(landtype, crop, flowerdens, datePeriod)) |>
  bind_rows(.id = "study")

lcflrcv <- lcflr |>
  group_by(study) |>
  mutate(
    flr.max = max(flowerdens),
    flr.scale = flowerdens/max(flr.max)) |> ungroup() |>
  group_by(study, landtype, crop) |>
  mutate(
    flrtime = flowerdens*datePeriod
  ) |>
  summarize(
    flr.sum = sum(flowerdens),
    flr.mn = mean(flowerdens),
    flr.sd = sd(flowerdens),
    flr.cv = flr.sd/flr.mn,
    flr.stbl = 1/flr.cv,
    pktime = sum(flrtime)/flr.sum,
    pktime.scale = pktime/max(datePeriod),
    flr.mn.scale = mean(flr.scale),
    .groups = "drop")

write.csv(lcflrcv, "data/continuitystats_landcover.csv", row.names = FALSE)

# ----- Spatial clustering of Iverson data

iv.sp <- st_read("data/spatial/iverson_pts.geojson", quiet = TRUE) |> st_transform(crs="epsg:5070")
iv.sp.dist <- dist(st_coordinates(iv.sp))

iv.hc <- hclust(iv.sp.dist, method = "single")

iv.clust <- cutree(iv.hc, h = 1000)

iv.sp.clust <- iv.sp |> mutate(farm = as.character(iv.clust))  |>
  mutate(
    site = replace_values(
      site,
      "Conifer_For34a Danby" ~ "Conifer_For34 Danby",
      "MesicUpRem_For34b Arnot"~ "MesicUpRem_For34 Arnot"
    )
  )

# st_write(iv.sp.clust, "data/spatial/iverson_pts_clust1km.geojson")

# ----- buffer data

gf <- read.csv("data/cropdiv_gf2021.csv") |>
  mutate(site=as.character(transect)) |>
  mutate(farm = stringr::str_split_i(site, "\\.", 1))
hw <- read.csv("data/cropdiv_hw2025.csv") |>
  rename(datePeriod=period_3wk, site=SITE) |>
  mutate(farm = site)
iv <- read.csv("data/cropdiv5_iverson.csv") |>
  mutate(
    datePeriod =
      recode_values(date,
                    "2016-04-15"~1,
                    "2016-05-15"~2,
                    "2016-06-15"~3,
                    "2016-07-15"~4,
                    "2016-08-15"~5)) |>
  # separate_wider_delim(site,delim = " ", names = c(NA, "farm"),
  #                      too_many = "merge", cols_remove = FALSE) |>
  # mutate(farm = farm |> str_to_upper() |>
  #          str_replace_all("[^A-Z0-9]+", "") |> str_squish() |>
  #          str_trunc(11,ellipsis = "")) |>
  left_join(iv.sp.clust |> st_drop_geometry() |> select(site, farm))

alldf <- list(gf=gf,hw=hw,iv=iv) |> bind_rows(.id = "study")

write.csv(alldf, "data/continuitystats_data.csv", row.names = FALSE)

# ----- continuity statistics -----

all.mn <- alldf |>
  group_by(study) |>
  mutate(
    # stdy.max = max(total.flowers),
    stdy.sd = sd(total.flowers),
    stdy.mn = mean(total.flowers),
    # flr.1scale = total.flowers/max(stdy.max),
    flr.zscale = (total.flowers-stdy.mn)/stdy.sd) |>
  ungroup() |>
  group_by(study, site, farm, crop.rich, crop.shandiv, pct.natural) |>
  mutate(flrtime = total.flowers*datePeriod) |>
  summarize(flr.sum = sum(total.flowers),
            flr.mn = mean(total.flowers),
            flr.sd = sd(total.flowers),
            flr.cv = flr.sd/flr.mn,
            # flr.mn.1scale = mean(flr.1scale),
            flr.mn.zscale = mean(flr.zscale),
            pktime = sum(flrtime)/flr.sum,
            pktime.scale = pktime/max(datePeriod),
            .groups = "drop") |>
  group_by(study, farm) |>
  summarize(
    flr.mn = mean(flr.mn),
    flr.mn.zscale = mean(flr.mn.zscale),
    flr.cv = mean(flr.cv),
    pktime.scale = mean(pktime.scale),
    crop.shandiv = mean(crop.shandiv),
    pct.natural = mean(pct.natural),
    .groups = "drop"
  )

write.csv(all.mn, "data/continuitystats_finaldata.csv", row.names = FALSE)
