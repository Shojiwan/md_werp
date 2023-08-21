library(lubridate); library(raster)
remove(list = ls()); cat("\014")

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
# Path to model folder and source functions
# path        <- XX_PATH_XX
path        <- 'C:/001_MD_WERP/002_R/md_werp/r/hs2tfv/'
source(paste0(path, '003_R/999_model_functions.R'))
path_out    <- paste0(path, '001_HS/out/')
path        <- paste0(path, '004_DATA/')
data        <- readRDS(paste0(path, 'supporting_data.RData'))

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
# Calendar and time data
months <- data.frame(numr = 1 : 12, mnth = c('jan', 'feb', 'mar', 'apr', 'may',
                                             'jun', 'jul', 'aug', 'sep', 'oct',
                                             'nov', 'dec'))
julian_days <- data.frame(
  julian_day  = 1 : 365,
  julian_date = as.POSIXct(as.Date(1 : 365, origin = as.Date("2017-01-01")),
                           tz = 'Australia/Brisbane') - days(2) + hours(14),
  new_day     = c(182 : 365, 1 : 181),
  new_date    = as.POSIXct(as.Date(c(182 : 365, 1 : 181), 
                                   origin = as.Date("2017-01-01")),
                           tz = 'Australia/Brisbane') - days(2) + hours(14)
)
origin_EXC <- as.POSIXct('01-01-1900', '%d-%m-%Y', tz = 'Australia/Brisbane')
origin_TFV <- as.POSIXct('01-01-1990', '%d-%m-%Y', tz = 'Australia/Brisbane')

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
# Total potential solar above features (Heatsource). Is not actual or effective
# solar but use this to scale BOM solar to effective; and break BOM into hourly
# HS must be run for a whole year to be able to correlate seasonal switches
# between northern and southern hemispheres (heatsource is programmed for
# northern hemisphere)
SR_files <- list.files(path    = path_out, pattern = 'SR1')
for (i in 1 : length(SR_files)) {
  temp <- read.csv(paste0(path_out, SR_files[i]), skip = 6)[1 : 2]
  if (i == 1) {hs_swr_TP <- temp} else {hs_swr_TP <- rbind(hs_swr_TP, temp)} 
}
hs_swr_TP$Datetime <- round_date(
  as.POSIXct(hs_swr_TP$Datetime * 86400, origin = data[['origin']],
             tz = 'Australia/Brisbane') - days(2), 'hour'
)
# Fix dates (flip seasons); July 1 = Jan 1; Model is based on N. Hemishpere
# Also this is off by 4 hours for some reasons. Add these 4 hours to bottom
hs_swr_TP <- shift_solar_ts(df = hs_swr_TP, hours = 4)
# Calculate hourly SWR as a percentage of total daily SWR (for use with dis-
# aggregating the BOM daily solar)
names(hs_swr_TP) <- c('date', 'hs_solarrad_tot_potential')
hs_swr_TP$day_date <- floor_date(hs_swr_TP$date, 'day')
hs_swr_daily <- aggregate(hs_swr_TP$hs_solarrad_tot_potential,
                          by = list(hs_swr_TP$day_date),
                          FUN = 'sum')
hs_swr_TP <- merge(hs_swr_TP, hs_swr_daily, by.x = 'day_date', by.y = 'Group.1', 
                   all = T)
hs_swr_TP$hs_solarrad_percent_daily <- round(hs_swr_TP$hs_solarrad_tot_potential / 
                                     hs_swr_TP$x * 100, 1) 
hs_swr_TP <- hs_swr_TP[, c(2, 3, 5)]
data[['HS_SR1']] <- hs_swr_TP

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
# BOM solar radiation for Mulga Downs
solar_BOM <- readLines(
  con = paste0(path, 'BOM_short_wave_radiation_at_mulga_downs_stn_44054.txt')
)
solar_headers <- solar_BOM[36]
solar_headers <- unlist(strsplit(x = solar_headers, split = '\\s+'))
solar_BOM <- solar_BOM[38 : length(solar_BOM)]
solar_BOM <- data.frame(do.call('rbind', strsplit(x = solar_BOM, split = '\\s+')))
names(solar_BOM) <- solar_headers
solar_BOM <- solar_BOM[, c(1, 11)]
solar_BOM$Radn <- as.numeric(solar_BOM$Radn) * 277.778 # convert from J -> W
solar_BOM$Date <- as.POSIXct(solar_BOM$Date, '%Y%m%d', tz = 'Australia/Brisbane')
# divvy up (W m-2 per hour) based on Heatsource diurnal distributions
# First expand hs_swr_TP to hourly
years_BOM <- unique(year(solar_BOM$Date))
for (year in years_BOM) {
  temp_HS <- hs_swr_TP
  year(temp_HS$date) <- year
  if (year == years_BOM[1]) {
    hs_swr2BOM <- temp_HS
  } else {
    hs_swr2BOM <- rbind(hs_swr2BOM, temp_HS)
  }
}
hs_swr2BOM$date_2 <- floor_date(hs_swr2BOM$date, 'day')
solar_BOM <- merge(hs_swr2BOM[, c(1, 3, 4)], solar_BOM, by.x = 'date_2', 
                   by.y = 'Date', all.x = F, all.y = T)
solar_BOM$swr_hrly <- solar_BOM$hs_solarrad_percent_daily * solar_BOM$Radn / 100
solar_BOM <- solar_BOM[order(solar_BOM$date), ]
solar_BOM <- solar_BOM[, -c(1, 3, 4)]
data[['BOM_SR1']] <- solar_BOM

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
# Geographic data
mesh <- shapefile(paste0("C:/001_MD_WERP/002_R/md_werp/r/hs2tfv/004_DATA/",
                         "culgoa_012_mesh_check_R.shp"))
nodes_TW <- read.csv(paste0("C:/001_MD_WERP/002_R/md_werp/r/hs2tfv/004_DATA/",
                        "lcdata_50m_nodes.csv"))
nodes_TW <- nodes_TW[, 1 : 5]
nodes_shape <- shapefile(paste0("C:/001_MD_WERP/002_R/md_werp/r/hs2tfv/004_DATA/",
                         "culgoa_bank_TW_line_nodes_50m_wgs84.shp"))
nodes_shape <- nodes_shape@data


#2345678901234567890123456789012345678901234567890123456789012345678901234567890
# Save object 
data <- list(month = months, julian_days = julian_days, origin_EXC = orig,
             origin_TFV = orig_2
             HS_SR1 = hs_swr_TP, BOM_SR1 = solar_BOM, model_domain = mesh,
             nodes_tw = nodes_TW, nodes_sf = nodes_shape)
saveRDS(object = data, file = paste0(path, 'supporting_data.RData'))
