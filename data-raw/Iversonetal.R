library(tidyverse)
library(readxl)
library(sf)
library(terra)
library(exactextractr)

# 1) Floral density:

sitespath <- "data-raw/Iversonetal/Plots_Final_2016_27.xlsx"

site_sheets <- excel_sheets(sitespath)

sites <- list()
for(i in seq_along(site_sheets)){
  sites[[site_sheets[i]]] <- read_excel(sitespath,
                                        sheet = i, range = "A1:AL661",
                                        na=c("", 'tree', 'sap', 'Tree', 'Sap', '(sap)', '(tree)', 's', 't'), n_max=661) %>%
    select(!matches("in.flower|flowers|X|Bloom_guide|unidentified|notes")) %>%
    mutate(
      Genus = trimws(Genus),
      species = trimws(species)
    )
}

#1) Floral density:
fl.density <- read.csv("data-raw/Iversonetal/Flowers_29May2024.csv",
                       na.strings = c("#DIV/0!","NA"), skip=1, fileEncoding = "latin1") |>
  mutate(
    Genus = as.factor(trimws(Genus)),
    species = as.factor(trimws(species))
  )

# 2) Floral area:

fl.area <- read.csv("data-raw/Iversonetal/FloralArea_29May2024.csv", fileEncoding = "latin1") |>
  mutate(
    Genus = as.factor(trimws(Genus)),
    species = as.factor(trimws(species))
  )

# 3) Bloom dates:

bloom.date <- read.csv("data-raw/Iversonetal/bloom_period_29May2024.csv", skip=1, fileEncoding = "latin1") |>
  mutate(
    Genus = as.factor(trimws(Genus)),
    species = as.factor(trimws(species))
  )

# 4) sex ratios

sex_ratios <- read.csv("data-raw/Iversonetal/Kass_flowers_33_ratios_v4.csv", skip=1, fileEncoding = "latin1") |> # updated Oct 25, 2022
  mutate(
    Genus = as.factor(trimws(Genus)),
    species = as.factor(trimws(species))
  )

# 5) urban trees
urb_sp_peaks <- read.csv('data-raw/Iversonetal/urb_comp_sum_trees6.csv')[,2:5] |>
  mutate(binomial = replace_values(binomial,
                                   "Matteucia struthiopteris" ~ "Matteuccia struthiopteris",
                                   "Taxus sp." ~ "Taxus canadensis")) |>
  # flower density
  left_join(fl.density |>
              mutate(binomial = paste(Genus, species)) |>
              select(binomial, Insect_pollinated, Avg_Flrs_1m, Avg_Flrs_Tree),
            by = "binomial") |>
  # flower area
  left_join(fl.area |>
              mutate(binomial = paste(Genus, species)) |>
              select(binomial, area.per.flower.mm2),
            by = "binomial") |>
  # sex ratio
  left_join(sex_ratios |>
              mutate(binomial = paste(Genus, species)) |>
              select(binomial, Mult_factor_final),
            by = "binomial") |>
  mutate(Avg_Flrs_Tree_Sexed = Avg_Flrs_Tree * Mult_factor_final) |>
  # bloom date
  left_join(bloom.date |>
              mutate(binomial = paste(Genus, species)) |>
              select(Genus, species, binomial, Start_final, End_final),
            by = "binomial") |>
  # floral calculation - [Aaron] divide by 27161 to get num of trees per m2
  # (odd way of thinking about it, I know). There was a total of 27161 m2 of
  # sampled urban green habitat...so this is the average area of flowers of each
  # tree per m2 of habitat [don't worry, i convert to per ha by multiplying by
  # 10000 in the code where i make floral resource curves] <- KL: taken out
  mutate(
    peakflr.herb = Avg_Flrs_1m * avg_cov_urb,
    peakflr.tree = (num_trees * Avg_Flrs_Tree_Sexed)/27161, # divide by 27161 to get num of trees per m2
    peakflr = rowSums(across(c(peakflr.herb, peakflr.tree)), na.rm = TRUE)
  ) |>
  # floral area calculation - this gives total floral area per species per m2
  # (the summed area of all beds). divide by a million to get to m2 of floral
  # area [KL: I kept it at mm2 floral area]. But this is per m2 not per hectare.
  mutate(
    peakarea.herb = peakflr.herb * area.per.flower.mm2,
    peakarea.tree = peakflr.tree * area.per.flower.mm2,
    peakarea = rowSums(across(c(peakarea.herb, peakarea.tree)), na.rm = TRUE)
  ) |>
  # define bloom curve
  mutate(
    span = End_final - Start_final,
    peakjday = Start_final + (span/2),
    periodsd = span/6,
    multi.flr = peakflr/dnorm(peakjday, mean=peakjday, sd=periodsd),
    multi.area = peakarea/dnorm(peakjday, mean=peakjday, sd=periodsd)
  ) |>
  mutate(habitat = recode_values(is.na(num_trees), TRUE ~ "Garden_flrs", FALSE ~ "Urban_tree"))

# 6) crop flowers
crop_curves <- read.csv("data-raw/Iversonetal/Corn_soy_apple_strawberry_fullyear.csv", row.names = 1) |>
  mutate(across(d1:d365, ~ .x / (area.per.flower.mm2/(1000*1000)) / 10000)) |>  # this part is converting from m^2/ha to flower density
  rename(habitat = common) |>
  mutate(habitat = recode_values(
    habitat,
    "domestic strawberry" ~ "Crop_strawberry",
    "soybean" ~ "Crop_soybean",
    "apple" ~ "Crop_apple",
    "corn" ~ "Crop_corn"
  ))

# conversion coefficients
subplot_coef <- 1/10/100*250*40
plotA_coef <- 1*0.0083*10000
plotB_coef <- 1*0.0059*10000
plotC_coef <- 1*0.0009*10000
D_coef <- 1*0.0022*10000
E_coef <- 1*0.0007*10000

# function for converting plot data to peak values and add curve parameters
convert_sites <- function(dat,
                          subplot_=subplot_coef,
                          plotA_=plotA_coef,plotB_=plotB_coef,plotC_=plotC_coef,
                          D_=D_coef, E_=E_coef,
                          fldensity = fl.density,
                          flarea = fl.area,
                          bloomdate = bloom.date,
                          sexratios = sex_ratios
                          ){
  dat.out <- dat |>
    mutate(scientificName = paste(Genus, species)) |>
    rowwise() |>
    mutate(
      subplot_cov = rowSums(across(starts_with("subplot_")), na.rm = TRUE)*subplot_,
      plotA_cov = `Plot A`*plotA_,
      plotB_cov = `Plot B`*plotB_,
      plotC_cov = `Plot C`*plotC_,
      D_cov = D*D_,
      E_cov = E*E_,
      avg_cov = rowSums(across(all_of(c("subplot_cov", "plotA_cov", "plotB_cov", "plotC_cov", "D_cov", "E_cov"))),na.rm = TRUE)
    ) |> ungroup() |>
    # join flower attributes
    left_join(fldensity |> select(Genus, species, Insect_pollinated, Avg_Flrs_1m, Avg_Flrs_Tree), by = c("Genus", "species")) |>
    left_join(flarea |> select(Genus, species, area.per.flower.mm2), by = c("Genus","species")) |>
    left_join(bloomdate |> select(Genus, species, Start_final, End_final), by = c("Genus","species")) |>
    left_join(sexratios |> select(Genus, species, Mult_factor_final), by = c("Genus","species")) |>
    # multiply sex ratio by sex ratio factor
    mutate(Avg_Flrs_Tree_Sexed = Avg_Flrs_Tree * Mult_factor_final) |>
    # floral calculation - DIFFERS FROM AARON'S CODE HERE TO GET FLOWERS PER m^2 INSTEAD OF PER HA
    mutate(
      peakflr.herb = Avg_Flrs_1m * avg_cov / 10000,
      peakflr.tree = No_canopy_trees * 40 * Avg_Flrs_Tree_Sexed / 10000,
      peakflr = rowSums(across(c(peakflr.herb, peakflr.tree)), na.rm = TRUE)
    ) |>
    # floral area calculation
    mutate(
      peakarea.herb = peakflr.herb * area.per.flower.mm2,
      peakarea.tree = peakflr.tree * area.per.flower.mm2,
      peakarea = rowSums(across(c(peakarea.herb, peakarea.tree)), na.rm = TRUE)
    ) |>
    # define bloom curve
    mutate(
      span = End_final - Start_final,
      peakjday = Start_final + (span/2),
      periodsd = span/6,
      multi.flr = peakflr/dnorm(peakjday, mean=peakjday, sd=periodsd),
      multi.area = peakarea/dnorm(peakjday, mean=peakjday, sd=periodsd)
    )
  dat.out
}

# test <- convert_plots(plots[[1]])

site_sp_peaks <- lapply(sites, convert_sites)  # these are the site species values + curve params

# make site curves

# function to make julian day values
make_jday <- function(peakjday, periodsd, multi, start_jday, end_jday){
  jdayvals <- dnorm(1:365, mean = peakjday, sd = periodsd)*multi
  names(jdayvals) <- paste0("d",1:365)

  # make values beyond start and end date 0
  # if(!is.na(start_jday) && !is.na(end_jday)){
  #   jdayvals[seq_along(jdayvals) < start_jday] <- 0
  #   jdayvals[seq_along(jdayvals) > end_jday] <- 0
  # }

  list(jdayvals)
}

# calculate julian day flower COUNT values for each site (not area)
site_sp_flrs <- lapply(site_sp_peaks, function(dat){
  dat |> rowwise() |>
    mutate(
      flrs = make_jday(peakjday = peakjday, periodsd = periodsd,
                       multi = multi.flr,
                       start_jday = Start_final, end_jday = End_final)
      ) |>
    ungroup() |>
    unnest_wider(flrs) |>
    select(Genus, species, d1:d365)
})

site_sp_flrs.df <- bind_rows(site_sp_flrs, .id = "site") |>
  filter(rowSums(across(d1:d365))>0) # just save the sp that have flowers to save space

write.csv(site_sp_flrs.df, "data/florlc_iverson.csv", row.names = FALSE)

## calculate julian day flower COUNT values

urb_sp_flrs <- urb_sp_peaks |> rowwise() |>
  mutate(
    flrs = make_jday(peakjday = peakjday, periodsd = periodsd, multi = multi.flr,
                     start_jday = Start_final, end_jday = End_final)
  ) |>
  ungroup() |>
  unnest_wider(flrs) |>
  select(habitat, Genus, species, d1:d365)

urb_sp_area <- urb_sp_peaks |> rowwise() |>
  mutate(
    areas = make_jday(peakjday = peakjday, periodsd = periodsd, multi = multi.area,
                     start_jday = Start_final, end_jday = End_final)
  ) |>
  ungroup() |>
  unnest_wider(areas) |>
  select(habitat, Genus, species, d1:d365)

write.csv(urb_sp_flrs, "data/florlc_urban_iverson.csv", row.names = FALSE)

# Make land cover averages

gps <- read.csv("data-raw/Iversonetal/GPS_coords_allsites_Feb2018_3.csv") |>
  st_as_sf(coords = c("Long", "Lat"), crs = "epsg:4326") |>
  # st_transform(crs = st_crs(lcrast)) |>
  mutate(
    site = replace_values(
      site,
      "Conifer_For34a Danby" ~ "Conifer_For34 Danby",
      "MesicUpRem_For34b Arnot"~ "MesicUpRem_For34 Arnot"
    )
  )

site_flrs <- gps |> st_drop_geometry() |> select(site, habitat) |>
  full_join(site_sp_flrs.df, by = "site") |>
  group_by(site, habitat) |>
  summarize(across(d1:d365, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

write.csv(site_flrs, "data/flowerdens_site_iverson.csv", row.names = FALSE)

urb_flrs <- urb_sp_flrs |>
  group_by(habitat) |>
  summarize(across(d1:d365, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

write.csv(urb_flrs, "data/flowerdens_urban_iverson.csv", row.names = FALSE)

# reclassify by raster classes
rast_crosstab <- read.csv("data-raw/Iversonetal/Habitat_category_cross-reference20260907.csv")

# combined mesic upland remnand and successional
mesicup_flrs <- site_flrs |> filter(habitat %in% c("MesicUpRem", "MesicUpSuc")) |>
  mutate(habitat = "MesicUp") |> group_by(habitat) |>
  summarize(across(d1:d365, mean), .groups = "drop")

# developed land covers
developed_flrs <- site_flrs |> filter(habitat == "Developed") |>
  bind_rows(urb_flrs) |>
  group_by(habitat) |>
  summarize(across(d1:d365, mean), .groups = "drop")

# calculate proportion developed (lawn) and urban tree for developed low and med intensity
developed_medlo_flrs <- developed_flrs |>
  filter(habitat == "Developed") |>
  mutate(habitat = "Developed_low_intensity") |>
  mutate(across(d1:d365, \(x) x*0.36)) |>
  bind_rows(
    developed_flrs |>
      filter(habitat == "Urban_tree") |>
      mutate(habitat = "Developed_low_intensity") |>
      mutate(across(d1:d365, \(x) x*0.2229))
  ) |>
  bind_rows(
    developed_flrs |>
      filter(habitat == "Developed") |>
      mutate(habitat = "Developed_med_intensity") |>
      mutate(across(d1:d365, \(x) x*0.2406))
  ) |>
  bind_rows(
    developed_flrs |>
      filter(habitat == "Urban_tree") |>
      mutate(habitat = "Developed_med_intensity") |>
      mutate(across(d1:d365, \(x) x*0.0747))
  ) |> group_by(habitat) |>
  summarize(across(d1:d365, sum))

hab_flrs <- site_flrs |>
  group_by(habitat) |>
  summarize(across(d1:d365, \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>
  bind_rows(crop_curves |> select(habitat, d1:d365)) |>
  bind_rows(mesicup_flrs |> select(habitat, d1:d365)) |>
  bind_rows(developed_flrs |> filter(habitat == "Urban_tree")) |>
  bind_rows(developed_medlo_flrs) |>
  bind_rows(
    data.frame(habitat = NA) |>
      bind_cols(matrix(rep(0,365),nrow = 1) |>
                  as.data.frame() |> setNames(paste0("d",1:365)))
  )

hab_flrs_cross <- rast_crosstab |>
  left_join(hab_flrs, by = c("site_habitat" = "habitat")) |>
  filter(!is.na(Code)) |>
  group_by(Floral_resources_landcover, Group, Code) |>
  summarize(across(d1:d365, sum), .groups = "drop") |>
  mutate(across(d1:d365, ~ round(.x, 1)))

write.csv(hab_flrs_cross, "data/flowerdens_iverson.csv", row.names = FALSE)

# extract lc buffers
gps_prj <- gps |> st_transform(crs = st_crs(lcrast))
sf::st_write(gps_prj, "data/spatial/iverson_pts.geojson")

# download the layer and unzip (it is big so this must be done locally)

lcrast <- rast("data-raw/Iversonetal/final_cat_2016.tif")
# raster.key <- read.csv("data-raw/Iversonetal/iverson_landcover_codes.csv")

bufs <- list(
  buf250 = sf::st_buffer(gps_prj, 250),
  buf500 = sf::st_buffer(gps_prj, 500),
  buf750 = sf::st_buffer(gps_prj, 750),
  buf1000 = sf::st_buffer(gps_prj, 1000),
  buf1500 = sf::st_buffer(gps_prj, 1500),
  buf2000 = sf::st_buffer(gps_prj, 2000)
)


# extract landcover values
bufdf <- lapply(bufs,
                \(x){
                  exact_extract(lcrast, x) |>
                    lapply(\(x){
                      group_by(x, value) |>
                        summarize(area = sum(coverage_fraction*prod(terra::res(lcrast))))
                    }) |> setNames(gps_prj$site) |>
                    bind_rows(.id = "site") |>
                    left_join(hab_flrs_cross |> select(Code, Floral_resources_landcover), by = c("value"="Code")) |>
                    pivot_wider(id_cols = site, names_from = Floral_resources_landcover, values_from = area, values_fill = 0)
                }) |>
  bind_rows(.id="buffer")

write.csv(bufdf, "data/lcareas_iverson.csv", row.names = FALSE)
