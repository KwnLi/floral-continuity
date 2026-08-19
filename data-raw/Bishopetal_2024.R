# Bees go up, flowers go down: Increased resource limitation from late spring to
# summer in agricultural landscapes

# Bishop et al 2024
# bumblebees, solitary bees
# percent floral coverage
# landscape floral coverage
# cumulative floral coverage in landscape

msdir <- "data-raw/Bishopetal_2024"
bishop <- paste0(msdir,"/BHL_DataforZenodo_Bishopetal2023.xlsx")

excel_sheets(bishop)

bishop.data <- readxl::read_xlsx(bishop, sheet = "MainData")
bishop.site <- readxl::read_xlsx(bishop, sheet = "SiteData")

write.csv(bishop.data |>
            dplyr::select(Site, HabitatType, Flwr_cov_perc, JulianDay, Year, SumDD5),
          "data/florlc_bisetal2024.csv", row.names = FALSE)
