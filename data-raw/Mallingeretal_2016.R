# Diverse landscapes have a higher abundance and species richness of spring wild
# bees by providing complementary floral resources over bees’ foraging periods

# Mallinger et al. 2016

# measured the abundance and species richness of flowers within orchards, annual
# field crops, grasslands, and woodlands at each of the five sites and in each
# time period during 2013: before apple bloom, April 23–30, during apple bloom,
# May 13–20, and after apple bloom, June 3–10.

msdir <- "data-raw/Mallingeretal_2016"
mall.sitelc <- readxl::read_xlsx(file.path(msdir,"site_landcover.xlsx"))
mall.flrs <- read.csv(file.path(msdir, "habitat_floralabund.csv"))
mall.treesp <- read.csv(file.path(msdir, "treesp.csv"))
mall.beesp <- read.csv(file.path(msdir, "beesp.csv"))

mall.frper <- mall.flrs |> tidyr::pivot_longer(cols=woodland_p1:crop_p3,
                                               values_to = "abundance",
                                               names_sep = "_p",
                                               names_to = c("landcover","period")) |>
  mutate(period = as.numeric(period))

write.csv(mall.frper, "data/florlc_maletal2016.csv", row.names = FALSE)
