# Predicting landscape-scale native bumble bee habitat use over space, time, and forage availability
# Hemberger & Williams 2025

library(terra)
library(tidyverse)
library(exactextractr)

msdir <- "data-raw/HembergerWilliams_2025/data"

hw.site <- read.csv(paste0(msdir,"/floral_ra_model_filtered.csv"))
hw.pollen <- read.csv(paste0(msdir,"/pollen_analysis.csv"))

write.csv(hw.site, "data/site_hw2025.csv", row.names = FALSE)

### Import reclass data

floral.df <- read_csv(file.path(msdir,"floral_clean.csv"))

### Summarize floral values HERE IS WHERE THE FLORAL AMOUNTS + MONTH ARE
# floral.values.df <- floral.df %>%
#   filter(!is.na(plant.type)) %>%
#   group_by(period, landcover, plant.type) %>%
#   summarise(mn.flwr.den = mean(flowerdens, na.rm = TRUE),
#             sd.flwr.den = sd(flowerdens, na.rm = TRUE),
#             cv.flwr.den = sd.flwr.den / mn.flwr.den * 100)  # NOT a temporal continuity measure

# write.csv(floral.df, "data/florlc_hw2025.csv", row.names = FALSE)

# Data is available as floral density for transects of different land cover
# types at the different sampling periods. However, there are some duplicates of
# the period, site, land cover, and species combinations.

# Look at duplicates
nrow(floral.df) - nrow(distinct(floral.df, period, site, landcover, species))

examine.dupes <- janitor::get_dupes(floral.df, period, site, landcover, species)

# Some of these look like samples taken within the same site and period, but
# actually on different dates. There are still some duplicates within the same
# date, site, period, land cover, and species (41), meaning species counts taken
# at the same site, land cover, and day, or the same species. The floral density
# estimates are different within these duplicates, which suggests these are
# different measurements, not accidental duplications.

examine.dupes2 <- janitor::get_dupes(floral.df, date, period, site, landcover, species)

# Looking back into more base data, there aren't columns that differentiate
# these duplicates, so I will average them.

# hw.dir <- "data-raw/HembergerWilliams_2025"
#
# snflwr.df <- read_csv(file.path(hw.dir,"data/floral_survey_sn.csv")) %>%
#   janitor::get_dupes(site, date, period, landcover, species, plant.type)
#
# cropflwr.df <- read_csv(file.path(hw.dir,"data/floral_survey_conv.csv")) %>% rename(landcover = field.type) %>%
#   janitor::get_dupes(site, date, period, landcover, species, plant.type)
#
# orgflwr.df <- read_csv(file.path(hw.dir,"data/floral_survey_org.csv")) %>% rename(landcover = field.type) %>%
#   janitor::get_dupes(site, date, period, landcover, species, plant.type)
#
# # the dataset I have doesn't filter by Bombus sp.
# bombus.flwr.df <- read_csv(file.path(hw.dir,"data/neal_data-confirmation/floral_bombus_visited.csv")) %>%
#   filter(bombus_collected == "yes")
#
# floral.df <- snflwr.df %>%
#   bind_rows(orgflwr.df) %>%
#   bind_rows(cropflwr.df) #%>%
# #filter(species %in% bombus.flwr.df$code)


### Get total flower density by date and land cover type

# Steps:
#
# 1. Average duplicated species estimates within the same date (period), site,
# landcover
#
# 2. Sum floral densities across the same site, date, and landcover
#
# Alternative: just do step 2.
#
# It also looks like some "periods" include multiple sampling dates. I will
# average those too.
#
# Looking back at the original manuscript for this data (Williams et al. 2012),
# the data were collected every three weeks, which is why the periods aren't
# monthly.

# test <- floral.df |>
#   mutate(date = as.Date(date, format = "%m/%d/%y"),
#          week = as.numeric(format(date, "%U")))
#
# ggplot(test, aes(week, group = period, fill = period)) +
#   geom_histogram(binwidth = 1)

# standardize site names (de-duplicate)
library(refinr)

floral.df.sites <- floral.df %>%
  mutate(site_clean = site %>%
           str_to_lower() %>%
           str_replace_all("&", " and ") %>%
           str_replace_all("@", " and ") %>%
           str_replace_all("at mile", " and ") %>%
           str_replace_all("southof", " s of ") %>%
           str_replace_all("'", " ") %>%
           str_replace_all("rd", " ") %>%
           str_replace_all("[^a-z0-9]+", " ") %>%    # strip punctuation
           str_squish()                              # collapse whitespace, trim
  ) %>%
  mutate(site_cluster = key_collision_merge(site_clean)) %>%
  mutate(site_cluster = n_gram_merge(site_cluster, numgram = 2))

review <- floral.df.sites %>%
  distinct(site, site_cluster) %>%
  arrange(site_cluster)


# average floral density by de-duplicated site (site_cluster)

lcflr.hw0 <- floral.df |>
  left_join(review, by = "site") |>
  mutate(
    date = as.Date(date, format = "%m/%d/%y"),
    period = toupper(period)
    ) |>
  group_by(date, period, site_cluster, landcover, species) |>
  summarize(flowerdens = mean(flowerdens, na.rm = TRUE), .groups = "drop") |>
  group_by(date, period, site_cluster, landcover) |>
  summarize(flowerdens = sum(flowerdens, na.rm = TRUE), .groups = "drop") |>
  mutate(period = ifelse(period ==".", toupper(format(date, "%B")), period))

# Backwards-engineer original 5 sampling periods reported in the original Williams et al. 2012 pub.
# AI disclosure - I used AI to identify the 5 periods. This section contains AI-generated code.

ggplot(lcflr.hw0, aes(x = date, y = 0, color = period)) +
  geom_jitter(height = 0.1, alpha = 0.4) +
  theme_minimal() +
  labs(title = "Sampling dates (jittered)", y = NULL)

# ---- Gap-based breakpoints ----
# Pool unique dates across ALL sites, sort, find largest gaps
dates_all <- lcflr.hw0 %>% distinct(date) %>% arrange(date) %>% pull(date)

gap_df <- tibble(
  date_before = dates_all[-length(dates_all)],
  date_after  = dates_all[-1],
  gap_days    = as.numeric(dates_all[-1] - dates_all[-length(dates_all)])
)

# Inspect the largest gaps - these are your candidate period boundaries
gap_df %>% arrange(desc(gap_days)) %>%
  print(n = 10)

# Take the top (n_periods - 1) gaps as breakpoints
# ALWAYS eyeball the printed table above first - make sure you're not
# splitting on a single straggler date that should belong to a bout

n_periods <- 5
top_gaps <- gap_df %>%
  arrange(desc(gap_days)) %>%
  filter(
    date_before != "2003-02-20",    # separating a single sampling date in Feb.
    date_before != "2003-04-04",    # too early cutoff
    date_before != "2003-05-20"     # too late cutoff
  )  %>%
  slice(1:(n_periods - 1)) %>%
  mutate(cutpoint = date_before + gap_days / 2) %>%
  arrange(cutpoint)

breakpoints_gap <- top_gaps$cutpoint
breakpoints_gap

lcflr.hw <- lcflr.hw0 |>
  mutate(period_3wk = findInterval(date, breakpoints_gap) + 1)

# visualize
ggplot(lcflr.hw, aes(x = date, y = 0, color = as.factor(period_3wk))) +
  geom_jitter(height = 0.1, alpha = 0.4) +
  theme_minimal() +
  labs(title = "Sampling dates (jittered)", y = NULL)

# test duplication
testdupe <- lcflr.hw %>% janitor::get_dupes(site_cluster, period_3wk, landcover)  # 6 duplicates!
testdupe2 <- lcflr.hw %>% janitor::get_dupes(site_cluster, period, landcover)     # 84 duplicates!

# average total flower density when taken there are two sampling dates in one period
# then expand the sampling dates to all periods to include 0s
lcflr.hw.mn <- lcflr.hw |> group_by(period_3wk, site_cluster, landcover) |>
  summarize(flowerdens = mean(flowerdens), .groups = "drop",
            date = min(date)) |>
  complete(period_3wk, nesting(site_cluster, landcover), fill = list(flowerdens=0))

write.csv(lcflr.hw.mn, "data/flowerdens_site_hw2025.csv", row.names = FALSE)

# ---- land cover raster key

raster.key.df <- read_csv(file.path(msdir,"raster_key_all2.csv")) |>
  mutate(raster.code.floral = ifelse(landcover.floral == "tomatilllo", 60, raster.code.floral)) |>
  mutate(raster.code.floral = ifelse(landcover.floral == "tomato-y", 39, raster.code.floral))

floralraster.key <- raster.key.df |> filter(!is.na(raster.code)) |>
  select(raster.code.floral, landcover.floral) |> distinct(raster.code.floral, .keep_all = TRUE) |>
  mutate(raster.name = landcover.floral |>
           stringr::str_remove_all("[^A-Za-z]") |>
           toupper() |>
           abbreviate(method = "both.sides", minlength = 6)) |> select(raster.code.floral, raster.name)

lckey.hw <- raster.key.df |>
  left_join(floralraster.key, by = "raster.code.floral")

write.csv(lckey.hw, "data/spatial/hw2025_lc_key.csv", row.names = FALSE)

# land cover raster

yolo.lc.raster <- terra::rast(file.path(msdir,"parcel_data/yolo_lc_raster.tif"))
yolo.floral.raster <- terra::classify(yolo.lc.raster,
                                 rcl = raster.key.df %>%
                                   dplyr::select(raster.code, raster.code.floral) %>%
                                   dplyr::mutate(raster.code.floral = ifelse(raster.code==99,NA,raster.code.floral)))

levels(yolo.floral.raster) <- floralraster.key

writeRaster(yolo.floral.raster, "data/spatial/hw2025_lc.tif", overwrite = TRUE)

### Floral density curves of individual land covers

# There are redundant codes in the landcover classes of `lcflr.hw`. Use the
# raster key to consolidate them into landcover classes only found in the
# landcover raster.

lckey.hw <- read.csv("data/spatial/hw2025_lc_key.csv")
lcflr.hw.mn <- read.csv("data/flowerdens_site_hw2025.csv")

rastlcflr.hw <- lcflr.hw.mn |>
  left_join(lckey.hw |>
              distinct(landcover.floral, raster.name, raster.code.floral, landcover.4cat, raster.code.4cat),
            by = c("landcover" = "landcover.floral")) |>
  filter(!is.na(raster.name)) |>   # remove land covers not in raster
  group_by(period_3wk, raster.name, raster.code.floral, landcover.4cat, raster.code.4cat) |>
  summarize(flowerdens = mean(flowerdens),
            flowerdens.sd = sd(flowerdens), .groups = "drop") |>
  select(period_3wk, raster.name, flowerdens) |>
  left_join(lckey.hw |> filter(!is.na(raster.code) & landcover.4cat != "na") |>
              distinct(raster.name, .keep_all = TRUE),
            by = "raster.name")

write.csv(rastlcflr.hw, "data/flowerdens_hw2025_v2.csv", row.names = FALSE)


# points
hw_pts <- sf::st_read(file.path(msdir, "site_centers/bombus_sites.shp"))
sf::st_write(hw_pts, "data/spatial/hw2025_pts.geojson")

# buffers
hw_buf250 <- sf::st_buffer(hw_pts, 250)
hw_buf500 <- sf::st_buffer(hw_pts, 500)
hw_buf750 <- sf::st_buffer(hw_pts, 750)
hw_buf1000 <- sf::st_buffer(hw_pts, 1000)
hw_buf1500 <- sf::st_buffer(hw_pts, 1500)
hw_buf2000 <- sf::st_buffer(hw_pts, 2000)

# extract landcover values
bufdf <- lapply(list(hw_buf250,hw_buf500,hw_buf750,hw_buf1000,hw_buf1500,hw_buf2000),
                \(x){
                  exact_extract(yolo.floral.raster, x) |>
                    lapply(\(x){
                      group_by(x, value) |>
                        summarize(area = sum(coverage_fraction*prod(terra::res(yolo.floral.raster))))
                    }) |> setNames(hw_pts$SITE) |>
                    bind_rows(.id = "SITE") |>
                    left_join(floralraster.key, by = c("value"="raster.code.floral")) |>
                    pivot_wider(id_cols = SITE, names_from = raster.name, values_from = area, values_fill = 0)
                }) |>
  setNames(c("250","500","750","1000","1500","2000")) |>
  bind_rows(.id="buffer")

write.csv(bufdf, "data/lcareas_hw2025.csv", row.names = FALSE)
