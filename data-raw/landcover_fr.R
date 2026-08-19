library(tidyverse)

# Guezen & Forrest

guezen.fr <- read.csv("data-raw/GuezenForrest_2021/Floral_counts.csv") |>
  mutate(date = as.Date(date, origin = "1898-12-30"))

lc.guezen <-

# Hemberger & Williams 2025

hw.fr <- read.csv("data-raw/HembergerWilliams_2025/data/floral_clean.csv")

# Hemberger et al. 2023

# Data are taken as proportion of quadrats; not the same as density of inflorescences like other data
# This data means something different from density - incidence if averaged overall; if summed, something like richness

# hemetal.fr <- read.csv("data-raw/Hembergeretal_2023/habitat_floralabun.csv")
