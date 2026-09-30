# Guezen and Forrest 2021
library(tidyverse)

msdir <- "data-raw/GuezenForrest_2021"

guezen.data <- read.csv(paste0(msdir,"/Full_dataset.csv"), check.names = FALSE) |>
  dplyr::mutate(pointID = dplyr::row_number())
# guezenreadme <- read.csv(paste0(msdir,"/ReadMe_for_CSV_files.csv"))

guezen.fr <- read.csv(file.path(msdir, "Floral_counts.csv")) |>
  mutate(date = as.Date(date, origin = "1898-12-30"))

date.check <- guezen.fr |>
  left_join(guezen.data |> dplyr::select(datePeriod, date, site, transect),
            by = c("datePeriod", "site", "transect"))
all.equal(as.character(date.check$date.x), date.check$date.y)  # some NAs where transects not present in guezen.data

write.csv(guezen.fr, "data/florlc_gf2021.csv", row.names = FALSE)

# Data is available for flower density within quadrats. Each row is a separate
# species count, maybe for same quadrat. Quadrats are organized within transects
# of land cover types. Multiple transects fall within the same site. There
# aren't transects for each land cover type at every single datepoint because:
# “If sampling locations contained open flowers during more  than one sampling
# period, the same location was sampled in multiple time periods” (pg 3133).

# Duplicates - some species are reported twice within the same transect and
# quadrat. In some cases, it's with the same density, in others, they are
# different.

nrow(guezen.fr) - nrow(distinct(guezen.fr))

examine.dupes.gf <- janitor::get_dupes(guezen.fr, -count_per_m2)


# Like Hemberger & Williams 2025, I'll average same species within a quadrat.

lcflr.gf0 <- guezen.fr |> group_by(date, datePeriod, site, landtype, transect, quadrat, flower) |>
  summarize(count_per_m2 = mean(count_per_m2, na.rm = TRUE),   # takes average over same species in same quadrat
            .groups = "drop") |>
  group_by(date, datePeriod, site, landtype, transect, quadrat) |>
  summarize(count_per_m2 = sum(count_per_m2, na.rm = TRUE),       # total flower count (density) per quadrat
            .groups = "drop")  |>
  group_by(date, datePeriod, site, landtype, transect) |>
  summarize(count_mn = mean(count_per_m2),                 # mean quadrat floral density
            count_sd = sd(count_per_m2),                # sd of floral counts in transect
            count_n = n(),
            .groups = "drop")

### Floral density curves of individual land covers

lcflr.gf <- lcflr.gf0 |> group_by(datePeriod, landtype) |>
  summarize(flowerdens = mean(count_mn),
            .groups = "drop") |>
  complete(datePeriod, landtype, fill = list(flowerdens = 0)) |>
  mutate(crop = ifelse(landtype %in% c("forest", "semi-natural"), "natural", "crop"))

write.csv(lcflr.gf, "data/flowerdens_gf2021.csv", row.names = FALSE)

d# ------ spatial
spfiles <- list.files(file.path(msdir,"Guezen_Forrest_shapefiles_2021"),pattern="*.shp", full.names = TRUE)

lcfiles <- spfiles[!grepl("Transect",spfiles)]

guezenpoly <- lapply(lcfiles, \(x){
  lcsf <- sf::st_read(x) |>
    mutate(id = as.numeric(id),
           landType = as.character(landType)) |>
    select(id, landType)
  lcsf[sf::st_geometry_type(lcsf) %in% c("POLYGON", "MULTIPOLYGON"),]
}) |> dplyr::bind_rows() |> sf::st_make_valid() |>
  sf::st_cast("MULTIPOLYGON")

# 1. Drop exact duplicate geometries first (common cause of "invisible" stacked polygons)
guezenpoly <- guezenpoly[!duplicated(sf::st_geometry(guezenpoly)), ]

# 2. Union all polygon boundaries into one noded line network, then polygonize
#    into atomic, non-overlapping faces
lines <- guezenpoly |>
  sf::st_geometry() |>
  sf::st_boundary() |>
  sf::st_union()

faces <- sf::st_polygonize(lines) |>
  sf::st_collection_extract("POLYGON") |>
  sf::st_sf(geometry = _) |>
  sf::st_make_valid()

faces$face_id <- seq_len(nrow(faces))

# 3. One safe interior point per face (st_centroid can fall outside concave shapes)
face_pts <- sf::st_point_on_surface(faces)

# 4. Rank original polygons by priority — smallest area wins here;
#    swap in a landType-based rank if that's a better rule for your data
ordered <- guezenpoly[order(sf::st_area(guezenpoly)), ]
ordered$priority_rank <- seq_len(nrow(ordered))

# 5. For each face, find every original polygon it falls inside and keep the
#    highest-priority one
hits <- sf::st_intersects(face_pts, ordered)

best <- vapply(hits, function(idx) {
  if (length(idx) == 0) return(NA_integer_)
  idx[which.min(ordered$priority_rank[idx])]
}, integer(1))

faces$id       <- ordered$id[best]
faces$landType <- ordered$landType[best]
faces <- faces[!is.na(faces$id), ]

# 6. Re-dissolve adjacent faces sharing the same attributes back into single polygons
final <- faces |>
  dplyr::group_by(id, landType) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  sf::st_make_valid() |>
  sf::st_collection_extract("POLYGON") |>
  sf::st_cast("MULTIPOLYGON")

sf::st_write(final, "data/spatial/gf2021_lcclean.geojson")

# transect points

guezen_pts <- guezen.data |> dplyr::select(site, transect, long, lat) |> dplyr::distinct() |>
  sf::st_as_sf(coords = c("long","lat"), crs = "epsg:4326") |>
  sf::st_transform(crs = sf::st_crs(guezenpoly))

sf::st_write(guezen_pts, "data/spatial/gf2021_pts.geojson")

# make buffers
guezen_pts <- sf::st_read("data/spatial/gf2021_pts.geojson")
final <- sf::st_read("data/spatial/gf2021_lcclean.geojson")

guezen_buf250 <- sf::st_buffer(guezen_pts, 250)
guezen_buf500 <- sf::st_buffer(guezen_pts, 500)
guezen_buf750 <- sf::st_buffer(guezen_pts, 750)

# extract landcover values
bufdf <- lapply(list(guezen_buf250,guezen_buf500,guezen_buf750),
                \(x){
                  intersectx <- sf::st_intersection(final, x)

                  intersectx |> sf::st_drop_geometry() |>
                    mutate(area = as.numeric(sf::st_area(intersectx))) |>
                    group_by(transect, landType) |> summarize(area = sum(area), .groups = "drop") |>
                    pivot_wider(id_cols = transect, names_from = landType, values_from = area, values_fill = 0) |>
                    mutate(total = rowSums(across(-transect)))
                }
                ) |>
  setNames(c("250","500","750")) |>
  bind_rows(.id="buffer")

write.csv(guezen.data, "data/site_gf2021.csv", row.names = FALSE)
write.csv(bufdf, "data/lcareas_gf2021.csv", row.names = FALSE)
