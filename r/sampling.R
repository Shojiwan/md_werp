rm(list = ls()); cat("\014")
# Node distance:  50 m
# Sample distance: 8 m at 5 intervals from the CL in radial pattern of directions:
# at the node (TO), NE (T1), E (T2), SE (T3), ..., N (T8)
# This needs to be run as if mirrored in the northern hemisphere, but east/west
# stay the same -- S = N, NE is SE, NW = SW, etc. So, when creating the sample
# points, start from SE and move counter-clockwise to S
dir1 <- 'F:/Backup/005_model/001_SLZ/01_inputs/03_hs'
dir2 <- 'F:/003_gis/002_projData/008_MD/014_model/mesh'
dir3 <- 'F:/003_gis/001_allData/006_elevation/MD/md_werp/data'
hdr1 <- names(read.csv(paste0(dir1, '/morphology.csv')))
hdr2 <- names(read.csv(paste0(dir1, '/lcdata.csv')))
data <- read.csv(paste0(dir2, '/culgoa_TW1_pnts.csv'))
rast <- readRDS(paste0(dir3, '/rasters.RData'))

# STREAM_ID	Branch/Reach
# NODE_ID - Absolute within the system (don't start over with each reach)
# STREAM_KM	0.05 km
# ELEVATION	Varies sample per rasters
# GRADIENT	Varies
# BOTTOM_WIDTH	Varies but not important for shade
# CHANNEL_ANGLE_Z	Varies
# MANNINGS_n	0.03
# SED_THERMAL_CONDUCTIVITY	2
# SED_THERMAL_DIFFUSIVITY	0.0077
# SED_HYPORHEIC_THICKNESSS	0.1
# HYPORHEIC_PERCENT	0
# POROSITY	0.3

# Start working through the data inputs
names(data)[c(1, 4 : 6)] <- c(hdr1[c(1, 7)], hdr2[c(4, 5)])
data <- data[, c(1 : 7, 10)]
data <- data[order(data$STREAM_ID, data$distance), ]
data$NODE_ID <- 1 : nrow(data)
data$STREAM_KM <- data$distance / 1000
data$CHANNEL_ANGLE_Z # This needs to be recalculated based on above











