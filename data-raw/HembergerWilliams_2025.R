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
floral.values.df <- floral.df %>%
  filter(!is.na(plant.type)) %>%
  group_by(period, landcover, plant.type) %>%
  summarise(mn.flwr.den = mean(flowerdens, na.rm = TRUE),
            sd.flwr.den = sd(flowerdens, na.rm = TRUE),
            cv.flwr.den = sd.flwr.den / mn.flwr.den * 100)  # NOT a temporal continuity measure

write.csv(floral.df, "data/florlc_hw2025.csv", row.names = FALSE)

# List of data frames for each month
# floral.values.list <- floral.values.df %>%
#   left_join(raster.key.df %>%
#               dplyr::select(raster.code.floral, landcover.floral),
#             by = c("landcover" = "landcover.floral")) %>%
#   group_by(period) %>%
#   group_split()

# land cover raster
raster.key.df <- read_csv(file.path(msdir,"raster_key_all2.csv")) |>
  mutate(raster.code.floral = ifelse(landcover.floral == "tomatilllo", 60, raster.code.floral)) |>
  mutate(raster.code.floral = ifelse(landcover.floral == "tomato-y", 39, raster.code.floral))

yolo.lc.raster <- terra::rast(file.path(msdir,"parcel_data/yolo_lc_raster.tif"))
yolo.floral.raster <- terra::classify(yolo.lc.raster,
                                 rcl = raster.key.df %>%
                                   dplyr::select(raster.code, raster.code.floral) %>%
                                   dplyr::mutate(raster.code.floral = ifelse(raster.code==99,NA,raster.code.floral)))

floralraster.key <- raster.key.df |> filter(!is.na(raster.code)) |>
  select(raster.code.floral, landcover.floral) |> distinct(raster.code.floral, .keep_all = TRUE) |>
  mutate(raster.name = landcover.floral |>
           stringr::str_remove_all("[^A-Za-z]") |>
           toupper() |>
           abbreviate(method = "both.sides", minlength = 6)) |> select(raster.code.floral, raster.name)

levels(yolo.floral.raster) <- floralraster.key

write.csv(raster.key.df |>
            left_join(floralraster.key, by = "raster.code.floral"),
          "data/spatial/hw2025_lc_key.csv", row.names = FALSE)

writeRaster(yolo.floral.raster, "data/spatial/hw2025_lc.tif", overwrite = TRUE)

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
