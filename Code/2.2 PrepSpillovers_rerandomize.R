# get data
cdcdata$LOTT_BIN <- unclass(factor(cdcdata$LOTT_BIN))

# extract adjacency matrix. Note that this is 1020 * 1020: there are 100 obs with no geo data
adj <- idv_dists[,4:ncol(idv_dists)]
dim(adj)

# Add in treatment data
# gps <- idv_dists[,-3]
gps <- GPS[,-3] #old code
gps <- merge(gps, cdcdata, by = "IDV")

# Rerandomize -- same # treated CDC in lottbin (ie conditional on geographic data availability)
cdc_level_data <- aggregate(x = gps[,c("TUUNGANE", "CDCCODE", "LOTT_BIN")], by = list(gps$CDCCODE), FUN = "mean")
resample       <- function() ave(cdc_level_data$TUUNGANE, cdc_level_data$LOTT_BIN, FUN = function(x)  sample(x))
rerand         <- function(){
  resampled <- cdc_level_data$CDCCODE[resample()==1]
  as.vector(gps$CDCCODE %in% resampled)
}

# Save in indirect measures
gps$indirect05  <- 1*indirect(d=5)
gps$indirect20 <- 1*indirect(d=20)

# Now get matrices of re-randomized exposure within some distance like this:
dir    <- replicate(spilloversims, rerand())
ind05  <- sapply(1:spilloversims, function(j) indirect(d = 5, treated = dir[,j]))
ind20  <- sapply(1:spilloversims, function(j) indirect(d = 20, treated = dir[,j]))

# 5km
# Back out probabilities and weights
gps$pDI_05  <- apply(dir     * ind05,     1, mean)
gps$pD0_05  <- apply(dir     * (1-ind05), 1, mean)
gps$p0I_05  <- apply((1-dir) * ind05,     1, mean)
gps$p00_05  <- apply((1-dir) * (1-ind05), 1, mean)

# Inverse weights
gps %<>%     dplyr::mutate(
  wDI_05  = 1/pDI_05,
  wD0_05  = 1/pD0_05,
  w0I_05  = 1/p0I_05,
  w00_05  = 1/p00_05
)

# 20km
# Back out probabilities and weights
gps$pDI_20  <- apply(dir     * ind20,     1, mean)
gps$pD0_20  <- apply(dir     * (1-ind20), 1, mean)
gps$p0I_20  <- apply((1-dir) * ind20,     1, mean)
gps$p00_20  <- apply((1-dir) * (1-ind20), 1, mean)

# Inverse weights
gps %<>%     dplyr::mutate(
  wDI_20  = 1/pDI_20,
  wD0_20  = 1/pD0_20,
  w0I_20  = 1/p0I_20,
  w00_20  = 1/p00_20
)

# Cleanup
# For full analysis there should only be cases that have a 0<p<1  probability of being in all conditions
gps %<>%     dplyr::mutate( 
  exclude05 = (pDI_05 ==1 | pDI_05 ==0) | (pD0_05 ==1 | pD0_05 ==0) | (p0I_05 ==1 | p0I_05 ==0) |(p00_05 ==1 | p00_05 ==0),
  exclude20 = (pDI_20 ==1 | pDI_20 ==0) | (pD0_20 ==1 | pD0_20 ==0) | (p0I_20 ==1 | p0I_20 ==0) |(p00_20 ==1 | p00_20 ==0)
)

# set weights to NA for excluded cases
# set weights to NA for excluded cases
gps$wDI_05[gps$exclude05] <- NA; gps$w0I_05[gps$exclude05] <- NA; gps$wD0_05[gps$exclude05] <- NA; gps$w00_05[gps$exclude05] <- NA
gps$wDI_20[gps$exclude20] <- NA; gps$w0I_20[gps$exclude20] <- NA; gps$wD0_20[gps$exclude20] <- NA; gps$w00_20[gps$exclude20] <- NA

gps$gps_weight05 <- gen_weight(D=gps$TUUNGANE, I=gps$indirect05, data = gps, km = "05")
gps$gps_weight20 <- gen_weight(D=gps$TUUNGANE, I=gps$indirect20, data = gps, km = "20")

gps <- merge(gps, villmeans2, by = "IDV", all.x = TRUE)

# Make weights matrices
w05  <- sapply(1:spilloversims, function(j) gen_weight(D=dir[,j], I=ind05[,j], data = gps, km = "05"))
w20  <- sapply(1:spilloversims, function(j) gen_weight(D=dir[,j], I=ind20[,j], data = gps, km = "20"))

# Export these datasets for use without gps information
gps <- select(gps, -starts_with("dist_")) %>% select(-c("latitude", "longitude"))


if(save_geodeidentified_data) write_rds(list(dir = dir, gps = gps), "Data/gps_masked/spillover_data_1.rds")
if(save_geodeidentified_data) write_rds(list(ind05 = ind05, ind20 = ind20), "Data/gps_masked/spillover_data_2.rds")
if(save_geodeidentified_data) write_rds(list(w05 = w05, w20 = w20), "Data/gps_masked/spillover_data_3.rds")


