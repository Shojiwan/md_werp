library(lubridate); library(ncdf4)
remove(list = ls()); cat("\014")
path <- 'C:/001_MD_WERP/001_Culgoa/002_TFV/culgoa_664/002_TFV/in/'

# Solar ----
fil1 <- nc_open(paste0(path, 'swr_09_sep.nc'))
fil2 <- nc_open(paste0(path, 'swr_01_jan.nc'))
tme1 <- as.POSIXct(fil1[["dim"]][["time"]][["vals"]] * 3600, 
                   origin = '1990-01-01', 
                   tz = 'Australia/Brisbane') - hours(11)
tme2 <- as.POSIXct(fil2[["dim"]][["time"]][["vals"]] * 3600, 
                   origin = '1990-01-01', 
                   tz = 'Australia/Brisbane') - hours(11)
# Need to check measurements and the time adjustment, but the obvious is that
# the year is wrong. Boo.
# ----

# Wind ----
fil1 <- nc_open(paste0(path, 'wind_09_sep.nc'))
fil2 <- nc_open(paste0(path, 'wind_01_jan.nc'))
tme1 <- as.POSIXct(fil1[["dim"]][["time"]][["vals"]] * 3600, 
                   origin = '1990-01-01', 
                   tz = 'Australia/Brisbane') - hours(11)
tme2 <- as.POSIXct(fil2[["dim"]][["time"]][["vals"]] * 3600, 
                   origin = '1990-01-01', 
                   tz = 'Australia/Brisbane') - hours(11)
# Year is off, too.
# ----

# Air temp ----
fil1 <- nc_open(paste0(path, 'tAir_09_sep.nc'))
fil2 <- nc_open(paste0(path, 'tAir_01_jan.nc'))
tme1 <- as.POSIXct(fil1[["dim"]][["time"]][["vals"]] * 3600, 
                   origin = '1990-01-01', 
                   tz = 'Australia/Brisbane') - hours(11)
tme2 <- as.POSIXct(fil2[["dim"]][["time"]][["vals"]] * 3600, 
                   origin = '1990-01-01', 
                   tz = 'Australia/Brisbane') - hours(11)
# Correct year, yay!
# ----


