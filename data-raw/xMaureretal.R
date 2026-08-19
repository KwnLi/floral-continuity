# Maurer et al 2024 (from Ammann et al. 2024)
msdir <- "data-raw/Maureretal_2024"

flowerdata <- read.csv(paste0(msdir,"/FlowerData.csv"))
interactdata <- read.csv(paste0(msdir,"/InteractionData.csv"))
landscapedata <- read.csv(paste0(msdir,"/LandscapeData.csv"), sep=";")


# Notes
# Either there's been a misprint or the data is not available for Ammann et al.
# The Dryad link for Ammann et al. 2024 leads to the data for Maurer et al. 2024
# In the data there's no way to relate the flower data to detailed land uses shown in the paper.
# This supplemental data only differentiates between intensive agriculture and rural habitat mosaic
