# MD-WERP 9.2 Water Quality Modelling; Tuflow FV WQ 
# Ryan Shojinaga & David Hamilton, Australian Rivers Institute, Griffith Uni.
# ryan.shojinaga@griffithuni.edu.au

# Script reads in solar outputs from Heatsource and creates a netcdf file for
# Tuflow FV. These are the Heatsource outputs:
# SR1 is the solar above veg and topography (atmospheric obstruction only)
# SR4 is the solar at the surface of the water (but not in the water)
# These will be used to create effective solar percentage reaching the water
# surface, which will then be applied to a BOM solar timeseries. 

library(ggplot2); library(reshape2); library(lubridate); library(raster)
library(oce); library(ncdf4)
remove(list = ls()); cat("\014")
# Path to model folder and source functions
path         <- 'C:/001_MD_WERP/001_Culgoa/002_TFV/culgoa_711/'
source(paste0(path, '003_R/999_model_functions.R'))

# READ IN DATA, FILES AND INFORMATION ----
control_file <- readLines(paste0(path, 'culgoa_cf.mst'))
# model_months <- month(clean_input(control_file[grep('start_date',
#                                                     control_file)])) :
#                 month(clean_input(control_file[grep('end_date',
#                                                     control_file)]))
model_months <- c(8 : 12, 1, 2)
model_year   <- year(clean_input(control_file[grep('start_date', control_file)]))
origin_TFV   <- as.POSIXct('1990-01-01', '%Y-%m-%d', tz = 'Australia/Brisbane')
origin_EXC   <- as.POSIXct('1900-01-01', '%Y-%m-%d', tz = 'Australia/Brisbane')
path_hs_out  <- paste0(path, '001_HS/out/')
hs_output    <- list.files(path_hs_out, pattern = 'SR4')
# All other data -- includes model mesh, stream nodes, solar data (HS and BOM)
data <- readRDS(paste0(path, '004_DATA/supporting_data_20240219.RData'))
# ----

# READ IN HEATSOURCE SOLAR AT WSE (SR4) ----
# for (month in model_months) { 
for (i in 1 : length(model_months)) {
  SR4_temp <- read.csv(paste0(path_hs_out, hs_output[i]), skip = 6)
  SR4_temp <- shift_solar_ts(df = SR4_temp, hours = 4)
  if (i == 1) {
    SR4 <- SR4_temp
  } else {
    SR4 <- rbind(SR4, SR4_temp)
  }
}
SR4$Datetime <- round_date(
  as.POSIXct(SR4$Datetime * 86400, origin = origin_EXC, 
             tz = 'Australia/Brisbane') - days(2), 'hour'
)
names(SR4)[2 : length(SR4)] <- gsub('X', 'SR4_', names(SR4)[2 : length(SR4)])
# Create data frame of effective shade based on HS SR1 and SR4, which will then
# be used to reduce solar at the nodes which will be translated into grids/ncdf
SR1 <- data[['HS_SR1']]
SR1_BOM <- data[['BOM_SR1']]
SR1 <- SR1[which(SR1$date %in% SR4$Datetime), ]
SR1_BOM <- SR1_BOM[which(SR1_BOM$date %in% SR4$Datetime), ]
# year(SR1$date) <- year(SR4$Datetime) <- model_year
eff_shade <- SR4_BOM <- SR4
for (node in 2 : length(SR4)) {
  cond <- which(SR1$hs_solarrad_tot_potential != 0)
  eff_shade[cond, node] <- eff_shade[cond, node] / 
                           SR1$hs_solarrad_tot_potential[cond]
  SR4_BOM[cond, node] <- eff_shade[cond, node] * SR1_BOM[cond, 2]
}
# ----

# Remove night-time timessteps except for bookend zero timesteps ----
SR1_BOM$keep <- 0
for (i in 2 : (nrow(SR1_BOM) - 1)) {
  if (SR1_BOM$swr_hrly[i] != 0) {
    SR1_BOM$keep[i] <- 1
  } else {
    if (SR1_BOM$swr_hrly[i - 1] != 0 | SR1_BOM$swr_hrly[i + 1] != 0) {
      SR1_BOM$keep[i] <- 1
    }
  }
}
SR4_BOM <- SR4_BOM[which(SR1_BOM$keep == 1), ]
SR1_BOM <- SR1_BOM[which(SR1_BOM$keep == 1), ]
# ----

# CREATE RASTERS AND NETCDF FILES ----
cSze <- 20 # Grid cell size
tmpG <- raster(xmn = as.integer(extent(data[['model_domain']])@xmin / cSze) * 
                 cSze + cSze / 2, 
               xmx = (as.integer(extent(data[['model_domain']])@xmax / cSze) + 1) *
                 cSze + cSze / 2, 
               ymn = as.integer(extent(data[['model_domain']])@ymin / cSze) * 
                 cSze + cSze / 2, 
               ymx = (as.integer(extent(data[['model_domain']])@ymax / cSze) + 1) * 
                 cSze + cSze / 2,
               crs = data[['model_domain']]@proj4string, resolution = c(cSze, cSze), 
               vals = NA)
# Create a mask for cells outside of domain
mask <- buffer(x = data[['model_domain']], width = 20)
nodes <- data[['nodes']][, c(1, 3, 6, 7)]
tmpS <- data.frame(
  RKM = round(as.numeric(gsub('SR4_', '', 
                              names(SR4_BOM)[2 : length(SR4_BOM)])), 2)
)
# Geographic NCDF dimensions and variables
x    <- xFromCol(tmpG, 1 : ncol(tmpG))
y    <- yFromRow(tmpG, 1 : nrow(tmpG))
xDim <- ncdim_def(name = "ni", units = "m", longname = "meters_east", 
                  vals = as.double(as.array(x)))
yDim <- ncdim_def(name = "nj", units = "m", longname = "meters_north", 
                  vals = as.double(as.array(y))) 
east <- ncvar_def(name = 'east', units = 'm', dim = list(xDim), 
                  missval = -99, longname = 'Easting', prec = 'float')
nrth <- ncvar_def(name = 'nrth', units = 'm', dim = list(yDim), 
                  missval = -99, longname = 'Northing', prec = 'float')

for (month in model_months) {
  # Create raster objects 
  rstr <- list() 
  SR1_temp <- SR1_BOM[which(month(SR1_BOM$date) == month), ]
  SR4_temp <- SR4_BOM[which(month(SR4_BOM$Datetime) == month), ]
  for (t in 1 : length(SR4_temp$Datetime)) {
    if (month %in% 1 : 2) {
      DATE <- SR4_temp$Datetime[t] + years(3)
    } else {
      DATE <- SR4_temp$Datetime[t] + years(2)
    }
    # DATE <- SR4_temp$Datetime[t]
    tmpE <- cbind(tmpS, SR4_temp = unlist(SR4_temp[t, 2 : length(SR4_temp)]))
    tmpE <- merge(nodes, tmpE, by.x = 'STREAM_KM', by.y = 'RKM', all = T)
    rstr[[t]] <- rasterize(x = cbind(tmpE$east, tmpE$nrth), y = tmpG,
                           field = tmpE$SR4_temp, fun = mean,
                           background = SR1_temp$swr_hrly[t])
    rstr[[t]] <- mask(x = rstr[[t]], mask = mask, inverse = F)
    names(rstr)[t] <- format(DATE, '%Y-%b-%d %H:%M')
  }
  if (month == 12) {
    saveRDS(object = rstr, 
            file = paste0(path, '002_TFV/in/swr_', 
                          ifelse(data[['month']]$numr[month] < 10, '0', ''),
                          data[['month']]$numr[month], '_', 
                          data[['month']]$mnth[month], '.RData'))
  }
  # CREATE AND POPULATE THE NCDF
  if (month %in% 1 : 2) {
    year(SR1_temp$date) <- year(SR1_temp$date) + years(3)
  } else {
    year(SR1_temp$date) <- year(SR1_temp$date) + years(2)
  }
  time <- as.numeric(SR1_temp$date - origin_TFV) * 24
  tDim <- ncdim_def(name = "time", units = 'hour',
                    vals = as.double(as.array(time)),
                    longname = 'Time in decimal hours since 01/01/1990 00:00')
  ncFl <- paste0(path, '002_TFV/in/swr_',
                 ifelse(data[['month']]$numr[month] < 10, '0', ''),
                 data[['month']]$numr[month], '_', data[['month']]$mnth[month],
                 '.nc')
  swr  <- ncvar_def(name = 'swr', units = 'W m^-2', dim = list(xDim, yDim, tDim),
                    missval = -99, longname = 'Short wave radiation',
                    prec = 'float')
  # create netCDF file and put arrays
  ncout <- nc_create(ncFl, list(east, nrth, swr), force_v4 = T)
  # put variables
  rArr <- array(NA, dim = c(dim(rstr[[1]])[1 : 2], length(time))) # rows, cols, time
  eArr <- array(x, dim = dim(rstr[[1]])[2])
  nArr <- array(y, dim = dim(rstr[[1]])[1]) #
  for (j in 1 : length(time)) {rArr[, , j] <- values(rstr[[j]])}
  ncvar_put(ncout, east, eArr)
  ncvar_put(ncout, nrth, nArr)
  ncvar_put(ncout,  swr, rArr)
  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"east","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"nrth","axis","Y")
  ncatt_put(ncout,"time","axis","T")
  # add global attributes
  ncatt_put(ncout,0,"title", 'MD-WERP, 9.2 Ecyhydrology Modelling')
  ncatt_put(ncout,0,"institution", 'GRIFFITH UNIVERSITY')
  ncatt_put(ncout,0,"contact", 'ryan.shojinaga@griffithuni.edu.au')
  ncatt_put(ncout,0,"source", 'ESTIMATED')
  # Close file
  nc_close(ncout)
}

# tmpX <- setValues(tmpG, sample(1 : 20, length(tmpG), replace = T))
windows(12, 12)
plot(rstr[[5]]); plot(data[['model_domain']], add = T)



