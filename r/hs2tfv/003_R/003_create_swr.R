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
path         <- 'C:/001_MD_WERP/001_Culgoa/002_TFV/meno_001/'
source(paste0(path, '003_R/999_model_functions.R'))
model_months <- c(7 : 12, 1 : 4)
model_year   <- 2023
origin_TFV   <- as.POSIXct('1990-01-01', '%Y-%m-%d', tz = 'Australia/Brisbane')
origin_EXC   <- as.POSIXct('1900-01-01', '%Y-%m-%d', tz = 'Australia/Brisbane')
path_hs_out  <- paste0(path, '001_HS/out/')
domn <- shapefile(paste0(path, '002_TFV/x/meno_test_p1_mesh_check_R.shp'))
nodes <- readRDS(paste0(path, '001_HS/in/radSmpl_data_meno_50m_nodes.RData'))
# nodes <- data[['nodes_sf']][, c('nNdx', 'east', 'nrth')] # index, east, nrth

# Read in SWR from ERA5 ----
SR1_BOM <- read.csv(paste0(path, '002_TFV/in/meno_001_met.csv'))[c(1, 6)]
SR1_BOM$DATE <- as.POSIXct(SR1_BOM$DATE, '%d/%m/%Y %H:%M', 
                           tz = 'Australia/Brisbane')
# ----

# READ IN HEATSOURCE SOLAR AT WSE (SR4) ----
hs_SR1       <- list.files(path_hs_out, pattern = 'SR1_')
hs_SR4       <- list.files(path_hs_out, pattern = 'SR4_')
for (month in model_months) { 
  SR1_temp <- read.csv(paste0(path_hs_out, hs_SR1[month]), skip = 6)[1 : 2]
  SR4_temp <- read.csv(paste0(path_hs_out, hs_SR4[month]), skip = 6)
  SR1_temp <- shift_solar_ts(df = SR1_temp, hours = 4)
  SR4_temp <- shift_solar_ts(df = SR4_temp, hours = 4)
  if (month == model_months[1]) {
    SR1 <- SR1_temp
    SR4 <- SR4_temp
  } else {
    SR1 <- rbind(SR1, SR1_temp)
    SR4 <- rbind(SR4, SR4_temp)
  }
}
SR4$Datetime <- round_date(
  as.POSIXct(SR4$Datetime * 86400, origin = origin_EXC, 
             tz = 'Australia/Brisbane') - days(2), 'hour'
)
year(SR4$Datetime) <- ifelse(month(SR4$Datetime) %in% 1 : 4, 
                             year(SR4$Datetime) + 1, 
                             year(SR4$Datetime))
SR1$Datetime <- SR4$Datetime
names(SR4)[2 : length(SR4)] <- gsub('X', 'SR4_', names(SR4)[2 : length(SR4)])
names(SR1)[2] <- 'hs_solarrad_tot_potential'

# Trim to met data and add a day (29/02) (use data from 1/03)
SR1 <- SR1[which(SR1$Datetime >= min(SR1_BOM$DATE) & 
                   SR1$Datetime <= max(SR1_BOM$DATE)), ]
SR4 <- SR4[which(SR4$Datetime >= min(SR1_BOM$DATE) & 
                   SR4$Datetime <= max(SR1_BOM$DATE)), ]
SR1 <- merge(SR1_BOM, SR1, by.x = 'DATE', by.y = 'Datetime', all = T)
SR4 <- merge(SR1_BOM, SR4, by.x = 'DATE', by.y = 'Datetime', all = T)
SR1 <- SR1[-which(month(SR1$DATE) == 6), c(1, 3)]
SR4 <- SR4[-which(month(SR4$DATE) == 6), c(1, 3 : length(SR4))]
cnd1 <- which(month(SR1$DATE) == 2 & day(SR1$DATE) == 28)
cnd2 <- which(month(SR1$DATE) == 2 & day(SR1$DATE) == 29)
SR1$hs_solarrad_tot_potential[cnd2] <- SR1$hs_solarrad_tot_potential[cnd1]
SR4[cnd2, 2 : length(SR4)] <- SR4[cnd1, 2 : length(SR4)]
SR1_BOM <- SR1_BOM[which(SR1_BOM$DATE >= min(SR1$DATE)), ]

# Create data frame of effective shade based on HS SR1 and SR4, which will then
# be used to reduce solar at the nodes which will be translated into grids/ncdf
# SR1 <- data[['HS_SR1']]
eff_shade <- SR4_BOM <- SR4
cond <- which(SR1$hs_solarrad_tot_potential != 0)
for (node in 2 : length(SR4)) {
  eff_shade[cond, node] <- eff_shade[cond, node] / 
                           SR1$hs_solarrad_tot_potential[cond]
  eff_shade[cond, node] <- ifelse(eff_shade[cond, node] < 0, 0,
                                  ifelse(eff_shade[cond, node] > 1, 1, 
                                         eff_shade[cond, node]))
  SR4_BOM[cond, node] <- eff_shade[cond, node] * SR1_BOM[cond, 2]
}
# ----

# Check these ----
# Randomly select 5 nodes during a seven-day period plot: 
# SR1, SR4, ES, SR1_BOM, SR4_BOM
# write.csv(x = nodes[["node"]], file = paste0(path_hs_out, 'nodes_meno_50m.csv'), 
#           row.names = F, quote = F)
# dte1 <- floor_date(SR4$DATE[sample(1 : nrow(SR4), 1)], 'day')
# dte2 <- dte1 + days(7)
# ndes <- sample(2 : length(SR4), 4)
# # ndes <- c(1913, 1407, 985, 797)
# dist <- nodes[['node']]$distance[which(nodes[['node']]$indx %in% ndes)]
# nmes <- data.frame(cols = 2 : length(SR4), dist = names(SR4)[2 : length(SR4)])
# nmes$dist <- as.numeric(gsub('SR4_', '', nmes$dist))
# cols <- nmes$cols[which(nmes$dist %in% dist)]
# chck <- data.frame(date = SR1$DATE, SR4[, cols], eff_shade[, cols],
#                    SR4_BOM[, cols])
# chck <- chck[which(chck$date >= dte1 & chck$date <= dte2), ]
# names(chck) <- c('date', paste0('SR4_HSXX', ndes), paste0('ESXX', ndes), 
#                  paste0('SR4_BOMXX', ndes))
# chk2 <- rbind(data.frame(SR1, node = ndes[1], srce = 'SR1_HS'),
#               data.frame(SR1, node = ndes[2], srce = 'SR1_HS'),
#               data.frame(SR1, node = ndes[3], srce = 'SR1_HS'),
#               data.frame(SR1, node = ndes[4], srce = 'SR1_HS'))
# chk3 <- rbind(data.frame(SR1_BOM, node = ndes[1], srce = 'SR1_BOM'),
#               data.frame(SR1_BOM, node = ndes[2], srce = 'SR1_BOM'),
#               data.frame(SR1_BOM, node = ndes[3], srce = 'SR1_BOM'),
#               data.frame(SR1_BOM, node = ndes[4], srce = 'SR1_BOM'))
# names(chk2)[1 : 2] <- names(chk3)[1 : 2] <- c('date', 'valu')
# chk2 <- chk2[which(chk2$date >= dte1 & chk2$date <= dte2), ]
# chk3 <- chk3[which(chk3$date >= dte1 & chk3$date <= dte2), ]
# chck <- melt(chck, id.vars = 'date', value.name = 'valu', variable.name = 'vars')
# chck <- cbind(chck, do.call('rbind', strsplit(as.character(chck$vars), 'XX')))
# names(chck)[4 : 5] <- c('srce', 'node')
# chck <- chck[, -2]
# chck <- rbind(chck, chk2, chk3)
# chck$srce <- factor(chck$srce, levels(factor(chck$srce))[c(3, 5, 1, 2, 4)])
# # windows(12, 12)
# ggplot(data = chck, aes(x = date, y = valu)) + geom_line() + theme_bw() +
#   facet_grid(srce ~ node, scales = 'free_y')
# Check these in GIS - Yup these look good
# ----

# Remove night-time timessteps except for bookend zero timesteps ----
SR1_BOM$keep <- 0
for (i in 2 : (nrow(SR1_BOM) - 1)) {
  if (SR1_BOM$SWRAD[i] != 0) {
    SR1_BOM$keep[i] <- 1
  } else {
    if (SR1_BOM$SWRAD[i - 1] != 0 | SR1_BOM$SWRAD[i + 1] != 0) {
      SR1_BOM$keep[i] <- 1
    }
  }
}
SR4_BOM <- SR4_BOM[which(SR1_BOM$keep == 1), ]
SR1_BOM <- SR1_BOM[which(SR1_BOM$keep == 1), ]
# ----

# CREATE RASTERS AND NETCDF FILES ----
cSze <- 50 # Grid cell size
tmpG <- raster(xmn = as.integer(extent(domn)@xmin / cSze) * 
                     cSze + cSze / 2, 
               xmx = (as.integer(extent(domn)@xmax / cSze) + 1) *
                     cSze + cSze / 2, 
               ymn = as.integer(extent(domn)@ymin / cSze) * 
                     cSze + cSze / 2, 
               ymx = (as.integer(extent(domn)@ymax / cSze) + 1) * 
                     cSze + cSze / 2,
               crs = domn@proj4string, resolution = c(cSze, cSze), 
               vals = NA)
# Create a mask for cells outside of domain
mask <- buffer(x = domn, width = 100)
nodes$STREAM_KM <- round(nodes$nNdx * 0.02, 2)
tmpS <- data.frame(
  RKM = round(as.numeric(gsub('SR4_', '', names(SR4_BOM)[2 : length(SR4_BOM)])), 2)
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
mnth <- data.frame(numr = 1 : 12, mnth = c('jan', 'feb', 'mar', 'apr', 'may', 
                                           'jun', 'jul', 'aug', 'sep', 'oct', 
                                           'nov', 'dec'))
cntr <- 1
datR <- list()
for (month in model_months) {
  # Create raster objects 
  rstr <- list() 
  SR1_temp <- SR1_BOM[which(month(SR1_BOM$DATE) == month), ]
  SR4_temp <- SR4_BOM[which(month(SR4_BOM$DATE) == month), ]
  for (t in 1 : length(SR4_temp$DATE)) {
    DATE <- SR4_temp$DATE[t]
    tmpE <- cbind(tmpS, SR4_temp = unlist(SR4_temp[t, 2 : length(SR4_temp)]))
    tmpE <- merge(nodes[['node']], tmpE, by.x = 'distance', by.y = 'RKM', all = T)
    rstr[[t]] <- rasterize(x = cbind(tmpE$east, tmpE$nrth), y = tmpG,
                           field = tmpE$SR4_temp, fun = mean,
                           background = SR1_temp$SWRAD[t])
    rstr[[t]] <- mask(x = rstr[[t]], mask = mask, inverse = F)
    names(rstr)[t] <- as.character(DATE)
  }
  datR[[cntr]] <- rstr
  names(datR)[cntr] <- mnth$mnth[which(mnth$numr == month)]
  cntr <- cntr + 1
  # CREATE AND POPULATE THE NCDF
  time <- as.numeric(SR1_temp$DATE - origin_TFV) * 24
  tDim <- ncdim_def(name = "time", units = 'hour', 
                    vals = as.double(as.array(time)),
                    longname = 'Time in decimal hours since 01/01/1990 00:00')
  ncFl <- paste0(path, '002_TFV/in/swr_', ifelse(mnth$numr[month] < 10, '0', ''), 
                 mnth$numr[month], '_', mnth$mnth[month], '.nc')
  swr  <- ncvar_def(name = 'swr', units = 'W m^-2', dim = list(xDim, yDim, tDim), 
                    missval = -99, longname = 'Short wave radiation', 
                    prec = 'float')
  # create netCDF file and put arrays
  ncout <- nc_create(ncFl, list(east, nrth, swr), force_v4 = T)
  # put variables # rows, cols, time
  rArr <- array(NA, dim = c(dim(rstr[[1]])[1 : 2], length(time)))
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
saveRDS(object = datR, file = paste0(path, '002_TFV/in/swr_meno.RData'))
