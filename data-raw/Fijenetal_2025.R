msdir <- "data-raw/Fijenetal_2025"

fijen.data <- read.csv(paste0(msdir,"/data.csv"))

write.csv(fijen.data |> dplyr::select(landscape, mfc.flower, log.av.flow.cov, round),
          "data/florlc_fijetal2025.csv",
          row.names = FALSE)
