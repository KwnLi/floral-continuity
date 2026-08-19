library(tidyverse)

rawdir <- "data-raw"
useraw <- c("HembergerWilliams_2025.R",
            "Hembergeretal_2023.R",
            "GuezenForrest_2021.R",
            "Fijenetal_2024.R",
            "Bishopetal_2023.R",
            "Mallingeretal_2016.R")
lapply(file.path(rawdir,useraw), source)

# bishopdata
