

remove(list = ls()); cat("\014")
# Path to model folder and source functions
# path        <- XX_PATH_XX
path        <- 'D:/Backup/005_model/008_MD/03_models/002_tuflow/culgoa_013_hcal/004_DATA'
path        <- paste0(path, '004_DATA/')


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

solr <- readLines(
  con = paste0(path, 'BOM_short_wave_radiation_at_mulga_downs_stn_44054.txt')
)

saveRDS()
