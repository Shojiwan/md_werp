library(ggplot2); library(reshape2); library(lubridate); library(raster)
library(oce); library(ncdf4)
remove(list = ls()); cat("\014")
# Path to model folder and source functions
# path         <- XX_PATH_XX
# path       <- 'D:/Backup/005_model/008_MD/03_models/002_tuflow/culgoa_013_hcal/'
path <- 'C:/001_MD_WERP/002_R/md_werp/r/hs2tfv/'
path_hs_out  <- paste0(path, '001_HS/out/')
hs_output    <- list.files(path_hs_out, pattern = 'SR1')
data <- readRDS(paste0(path, '004_DATA/supporting_data.RData'))
source(paste0(path, '003_R/999_model_functions.R'))
for (i in 1 : 12) { # Have to read all months of solar in (to match seasonals)
  SR1_temp <- read.csv(paste0(path_hs_out, hs_output[i]), skip = 6)[1 : 2]
  # SR4_temp <- shift_solar_ts(df = SR4_temp, hours = 4)
  if (i == 1) {SR1 <- SR1_temp} else {SR1 <- rbind(SR1, SR1_temp)}
}
SR1$Datetime <- round_date(
  as.POSIXct(SR1$Datetime * 86400, origin = data[['origin']], 
             tz = 'Australia/Brisbane') - days(2), 'hour'
)
SR1_daily <- aggregate(SR1$X424.200, by = list(floor_date(SR1$Datetime, 'day')),
                       FUN = 'sum')
windows(12, 12)
ggplot(data = SR1_daily, aes(x = Group.1, y = x)) + geom_line() + theme_bw()

SR1_BU <- SR1

SR1 <- SR1_BU
SR1$date <- shift_solar_ts(SR1, hours = 5)
