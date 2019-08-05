
# Import and attach re-randomization datasets
masked_gps_1 <- read_rds("Data/gps_masked/spillover_data_1.rds")
masked_gps_2 <- read_rds("Data/gps_masked/spillover_data_2.rds")

dir   <- masked_gps_1$dir
ind05 <- masked_gps_1$ind05
w05   <- masked_gps_1$w05

gps   <- masked_gps_2$gps
ind20 <- masked_gps_2$ind20
w20   <- masked_gps_2$w20


