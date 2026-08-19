# Hemberger et al. 2023
msdir <- "data-raw/Hembergeretal_2023"

hemberger.site <- read.csv(paste0(msdir,"/floral/site_floral_estimates.csv"))
# hembergerbee1 <- read.csv(paste0(msdir,"/bumble/bombus_2017.csv"))
# hembergerbee2 <- read.csv(paste0(msdir,"/bumble/bombus_2018.csv"))
# hembergerbee3 <- read.csv(paste0(msdir,"/bumble/bombus_hannah.csv"))
hemberger.bees <- read.csv(paste0(msdir,"/bumble/bombus_relabun.csv"))

hemberger.fr <- read.csv(file.path(msdir,"habitat_floralabun.csv"))

write.csv(hemberger.fr, "data/florlc_hemetal2023.csv", row.names = FALSE)
