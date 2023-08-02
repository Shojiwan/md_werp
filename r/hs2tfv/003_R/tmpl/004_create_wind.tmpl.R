# MD-WERP 9.2 Water Quality Modelling; Tuflow FV WQ 
# Ryan Shojinaga & David Hamilton, Australian Rivers Institute, Griffith Uni.
# ryan.shojinaga@griffithuni.edu.au

# SCRIPT DESCRIPTION

remove(list = ls()); cat("\014")
# Path to model folder and source functions
path         <- XX_PATH_XX


path <- '//staff.ad.griffith.edu.au/ud/fr/s5281039/Desktop/lanosrep/md_werp/r/hs2tfv/'
source(paste0(path, '/003_R/999_model_functions.R'))
wind_data <- read.csv(paste0(path, '/004_DATA/StGeorgeAirport_043109_edited.csv'))



















library(ncdf4); library(ggplot2); library(reshape2); library(lubridate); 
library(raster)
remove(list = ls()); cat("\014")
# DATA ----
dir1 <- "D:/003_gis/002_projData/008_MD/003_ecology/"
dir2 <- "D:/003_gis/001_allData/003_ecology/"
dir3 <- "D:/Backup/005_model/008_MD/01_inputs/02_tuflow/met/ncfil/"
dir4 <- 'D:/Backup/004_data/002_projData/008_MD/'
dir5 <- 'D:/003_gis/001_allData/006_elevation/MD/'
# Set Dates 
tmeZ <- 'Australia/Brisbane'
# dtes <- as.POSIXct(c('2019-05-10', '2019-09-01'), '%Y-%m-%d', tz = tmeZ) 
# q <- read.csv(paste0(dir4, 'hydrology/flow_data/422208B_20230425/422208B.csv'),
#               skip = 3)[c(1, 4, 6, 10)]
# names(q) <- c('date', 'h_m', 'qcms', 't_C')
# q$date <- as.POSIXct(q$date, '%H:%M:%S %d/%m/%Y ', tz = tmeZ)
# q <- q[which(q$date >= dtes[1] & q$date <= dtes[2]), ]
# q <- melt(q, id.vars = 'date', variable.name = 'parm', value.name = 'valu')
# ggplot(data = q, aes(x = date, y = valu)) + geom_line() + scale_y_log10() +
#   facet_wrap(.~ parm, scales = 'free_y', ncol = 1)

# 345678901234567890123456789012345678901234567890123456789012345678901234567890
# Period 1 - Examine a period of flow within the reach 2019-05-01 to 2019-06-01
dtes <- as.POSIXct(c('2019-05-01', '2019-06-01'), '%Y-%m-%d', tz = tmeZ) 
#
# 345678901234567890123456789012345678901234567890123456789012345678901234567890

# Remnant and regrowth veg (2017)
remn <- shapefile(paste0(dir1, 'RE_2017_clip2.shp'))
svtm <- raster(paste0(dir1, 'svtm_nsw_clip2.tif'))
# regr # No going to include regrowth, only small bits in model domain
redd <- read.csv(paste0(dir2, "QLD/redd_v12_2021.csv"))
bioN <- read.csv(paste0(dir2, "NSW/CSVExport_PCT_export_all_20230625063422.csv"))
# Wind speed and direction -- from_UWA/MD WERP Data/Data
wind <- read.csv(paste0(dir4, 'from_UWA/MD WERP Data/Data/',
                        'StGeorgeAirport_043109_edited.csv'))[c(10, 19, 21)]
wind$Datetime <- as.POSIXct(wind$Datetime, '%d/%m/%Y %H:%M', tz = tmeZ)
# Wind at 10m (open wind)
wind <- wind[which(wind$Datetime >= dtes[1] & wind$Datetime <= dtes[2]), ]
wrf  <- read.csv(paste0(dir3 , "wrfs.csv"))
# Model domain
mesh <- shapefile(paste0("D:/Backup/005_model/008_MD/03_models/002_tuflow/",
                         "set_up/model/sms/culgoa_012_mesh_whole.shp"))
# mesh <- shapefile(paste0("D:/Backup/005_model/008_MD/03_models/002_tuflow/",
#                          "culgoa_004b_Hydros_SVarMat/check/culgoa_004_mesh",
#                          "_whole.shp"))
# ----

# TEMPLATE GRID ----
cSze <- 50 # Grid cell size
tmpG <- raster(xmn = as.integer(extent(mesh)@xmin / cSze) * cSze, 
               xmx = (as.integer(extent(mesh)@xmax / cSze) + 1) * cSze, 
               ymn = as.integer(extent(mesh)@ymin / cSze) * cSze, 
               ymx = (as.integer(extent(mesh)@ymax / cSze) + 1) * cSze,
               crs = mesh@proj4string, resolution = c(cSze, cSze), 
               vals = NA)
# Create a mask for cells outside of domain
mask <- buffer(x = mesh, width = 100)
# ----

# MEAN CANOPY ELEVATION CATEGORIZATION ----
# vFil <- list.files(path = paste0(dir5, 'point_cloud/culgoa/rstrs/'), 
#                    pattern = 'veg_H') # These are based on max veg height
# vFil <- vFil[grep('.asc', vFil)]
# vegH <- list()
# for (i in 1 : 8) {
#   vegH[[i]] <- raster(paste0(dir5, 'point_cloud/culgoa/rstrs/', vFil[i]))
#   vegH[[i]] <- aggregate(vegH[[i]], fact = 25, fun = mean, expand = F, 
#                          na.rm = TRUE)
# }
# vegH <- merge(vegH[[1]], vegH[[2]], vegH[[3]], vegH[[4]], vegH[[5]], vegH[[6]],
#               vegH[[7]], vegH[[8]])
# vegH <- resample(vegH, y = tmpG)
# writeRaster(filename = paste0(dir5, 'point_cloud/culgoa/rstrs/', 'vegH_50m.asc'), 
#             x = vegH, overwrite = T)
vegH <- raster(paste0(dir5, 'point_cloud/culgoa/rstrs/', 'vegH_50m.asc'))
df <- data.frame(rasterToPoints(vegH))
names(df)[3] <- 'layer'
# Correlate to veg group (from Moon etal 2019)
hgts <- data.frame(catg = c('a', 'b', 'd', 'e', 'f', 'g'),
                   hght = c(17, 25, 35, 2.1, 3.9, 49))
hgts <- hgts[order(hgts$hght), ]
df$cat <- ifelse(df$layer < hgts$hght[1], 1, # Cat e: Open scrub (< 2.1m) 
                 ifelse(df$layer < hgts$hght[2], 2, # Cat f: Open scrub (< 3.9m)
                        ifelse(df$layer < hgts$hght[3], 3, # Cat a: Open woodland (17m)
                               ifelse(df$layer < hgts$hght[4], 4, 1)))) # Cat b: regrowth (25m)
vegC <- rasterize(x = SpatialPoints(coords = df[, 1 : 2], 
                                    proj4string = mesh@proj4string), 
                  y = tmpG, field = df$cat)  
# ----

# ESTIMATE WIND SPEED VECTORS AT 1m ----
# Based on Moon etal (2019): Variation in wind ~ H and (u, v)_open_air
# Break wind data into 2D velocity vector; for better or worse, I will assume
# Direction is degrees from true north in CW direction (e.g., 0 & 360 = N; 90 = E)
names(wind) <- c('date', 'skph', 'dirD')
wind$hour <- hour(wind$date) + minute(wind$date) / 60
wind <- wind[which(wind$hour %% 2 == 0), ]
# thr <- wind[which(wind$hour %% 3 == 0), ]
wind <- wind[, -4]
wind$dirR <- wind$dirD * pi / 180
wind$smps <- round(wind$skph * 1000 / 3600, 3) # km/h -> m/s
wind$u <- round(wind$smps * sin(wind$dirR), 3) # Wind component in E-W direction
wind$v <- round(wind$smps * cos(wind$dirR), 3) # Wind component in N-S direction
# CHECK 
# test <- wind[sample(1 : nrow(wind), size = 20), ]
# # windows(12, 12)
# xLim <- ceiling(max(c(test$u, test$v), na.rm = T) * 1.1)
# ggplot(data = test, aes(x = u, y = v)) + geom_point(size = 2) + theme_bw() +
#   geom_text(aes(label = dir2), nudge_x = 1, nudge_y = 1) + 
#   facet_wrap(.~ Datetime, ncol = 5) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
#   scale_x_continuous(limits = c(-xLim, xLim)) + 
#   scale_y_continuous(limits = c(-xLim, xLim))
# GOOD
# Multiply these wind vectors by the wind reduction factors for each veg group
# wrf: 1 = e; 2 = f; 3 = a; 4 = b 
vegT <- c('e', 'f', 'a', 'b') 
for (i in c(2, 5 : 7)) wind[which(is.na(wind[, i])), i] <- 0
wind$redB <- wind$redA <- wind$redF <- wind$redE <- NA
for (i in 1 : nrow(wind)) {
  for (j in 1 : 4) {
    tmpR <- wrf[which(wrf$cat3 == vegT[j]), ]
    if (wind$skph[i] < tmpR$x2[1]) {
      wind[i, j + 7] <- tmpR$y2[1]
    } else if (wind$skph[i] > tmpR$x2[nrow(tmpR)]) { 
      wind[i, j + 7] <- tmpR$y2[nrow(tmpR)]
    } else {
      tmpR <- tmpR[max(which(tmpR$x2 <= wind$skph[i])) : 
                     min(which(tmpR$x2 >= wind$skph[i])), ] 
      wind[i, j + 7] <- tmpR$y2[1] + (wind$skph[i] - tmpR$x2[1]) *  
        (tmpR$y2[2] - tmpR$y2[1]) /  
        (tmpR$x2[2] - tmpR$x2[1]) 
    }
  }
}
# Cool, that worked
# Apply the wind reduction factors (WRF)
wind$u_e <- wind$redE * wind$u; wind$v_e <- wind$redE * wind$v
wind$u_f <- wind$redF * wind$u; wind$v_f <- wind$redF * wind$v
wind$u_a <- wind$redA * wind$u; wind$v_a <- wind$redA * wind$v
wind$u_b <- wind$redB * wind$u; wind$v_b <- wind$redB * wind$v
# No WRF application
# wind$u_e <- wind$u; wind$v_e <- wind$v
# wind$u_f <- wind$u; wind$v_f <- wind$v
# wind$u_a <- wind$u; wind$v_a <- wind$v
# wind$u_b <- wind$u; wind$v_b <- wind$v

# ----

# CREATE RASTERS ----
# wrf: 1 = e; 2 = f; 3 = a; 4 = b 
df1 <- data.frame(rasterToPoints(vegC))
df1 <- merge(df1, data.frame(vegC = 1 : 4, col1 = grep('u_', names(wind))), 
             by.x = 'layer', by.y = 'vegC', all = T)
wndR <- list(u = list(), v = list())
cntr <- seq(25, nrow(wind), 25)
for (i in 1 : nrow(wind)) {
  wnd1 <- data.frame(col1 = grep('u_', names(wind)),
                     u = unlist(wind[i, grep('u_', names(wind))]),
                     v = unlist(wind[i, grep('v_', names(wind))]))
  dfU <- merge(df1, wnd1[, c(1, 2)], by.x = 'col1', by.y = 'col1', all = T)
  dfV <- merge(df1, wnd1[, c(1, 3)], by.x = 'col1', by.y = 'col1', all = T)
  wndR[['u']][[i]] <- rasterize(x = cbind(dfU$x, dfU$y), y = tmpG, field = dfU$u, 
                                fun = mean, background = 0)
  wndR[['v']][[i]] <- rasterize(x = cbind(dfV$x, dfV$y), y = tmpG, field = dfV$v, 
                                fun = mean, background = 0)
  wndR[['u']][[i]] <- mask(x = wndR[['u']][[i]], mask = mask, inverse = F)
  wndR[['v']][[i]] <- mask(x = wndR[['v']][[i]], mask = mask, inverse = F)
  names(wndR[['u']])[i] <- as.character(wind$date[i], format('%Y-%m-%d %H:%M'))
  names(wndR[['v']])[i] <- as.character(wind$date[i], format('%Y-%m-%d %H:%M'))
  if (i %in% cntr) {
    a <- format(Sys.time(), '%H:%M')
    cat(paste0('Rows processed: ', i, ' at ', a, '\n'))
  }
}
saveRDS(object = wndR, file = paste0(dir3, 'wind_rstrs_05_2019_012.RData'))
# ----





library(ncdf4); library(ggplot2); library(reshape2); library(lubridate); 
library(raster)
remove(list = ls()); cat("\014")
# DATA ----
dir1 <- "D:/Backup/005_model/008_MD/01_inputs/02_tuflow/met/ncfil/"
wndR <- readRDS(paste0(dir1, 'wind_rstrs_05_2019_012.RData'))
dtes <- as.POSIXct(names(wndR[[1]]), '%Y-%m-%d %H:%M', tz = 'Australia/Brisbane')
# DATES 2019-05-01 to 2019-06-01 
mnth <- data.frame(mntR = 1 : 12, mntN = c('jan', 'feb', 'mar', 'apr', 
                                           'may', 'jun', 'jul', 'aug', 'sep', 
                                           'oct', 'nov', 'dec'))
# ----

# Create dimensions ----
x    <- xFromCol(wndR[[1]][[1]], 1 : ncol(wndR[[1]][[1]]))
y    <- yFromRow(wndR[[1]][[1]], 1 : nrow(wndR[[1]][[1]]))
xDim <- ncdim_def(name = "ni", units = "m", longname = "meters_east", 
                  vals = as.double(as.array(x)))
yDim <- ncdim_def(name = "nj", units = "m", longname = "meters_north", 
                  vals = as.double(as.array(y))) 
i = 5
# for (i in 5) { # 2 : 12) {
tmpD <- which(dtes %in% dtes)
# tmpR <- r[tmpD]
# Create NetCDF file 
dirN <- 'D:/Backup/005_model/008_MD/01_inputs/02_tuflow/met/ncfil'
# ncFl <- paste0(dirN, '/met_50m.nc')
ncFl <- paste0(dirN, '/wnd_50m_',  ifelse(mnth$mntR[i] < 10, '0', ''), 
               mnth$mntR[i], '_', mnth$mntN[i], '_2019_012.nc')
# define dimensions
tOrg <- as.POSIXct("1990-01-01", '%Y-%m-%d', tz = 'Australia/Brisbane')
t    <- as.numeric(dtes[tmpD] - tOrg) * 24
tDim <- ncdim_def(name = "time", units = 'hour', vals = as.double(as.array(t)),
                  longname = 'Time in decimal hours since 01/01/1990 00:00')
# define variables
wndU <- ncvar_def(name = 'u', units = 'm s^-1', dim = list(xDim, yDim, tDim),
                  missval = -9999, longname = 'Windspeed, E/W vector',
                  prec = 'float')
wndV <- ncvar_def(name = 'v', units = 'm s^-1', dim = list(xDim, yDim, tDim),
                  missval = -9999, longname = 'Windspeed, N/S vector',
                  prec = 'float')
east <- ncvar_def(name = 'east', units = 'm', dim = list(xDim), 
                  missval = -99, longname = 'Easting', prec = 'float')
nrth <- ncvar_def(name = 'nrth', units = 'm', dim = list(yDim), 
                  missval = -99, longname = 'Northing', prec = 'float')
# create netCDF file and put arrays
ncout <- nc_create(ncFl, list(east, nrth, wndU, wndV), force_v4 = T)
# put variables
uArr <- array(NA, dim = c(dim(wndR[['u']][[1]])[2 : 1], length(tmpD)))
vArr <- array(NA, dim = c(dim(wndR[['u']][[1]])[2 : 1], length(tmpD)))
eArr <- array(xFromCol(wndR[[1]][[1]], 1 : ncol(wndR[[1]][[1]])), 
              dim = c(dim(wndR[['u']][[1]])[2]))
nArr <- array(yFromRow(wndR[[1]][[1]], 1 : nrow(wndR[[1]][[1]])), 
              dim = c(dim(wndR[['u']][[1]])[1]))
for (j in 1 : length(tmpD)) {
  uArr[, , j] <- getValues(wndR[['u']][[j]])
  vArr[, , j] <- getValues(wndR[['v']][[j]])
}
ncvar_put(ncout, wndU, uArr)
ncvar_put(ncout, wndV, vArr)
ncvar_put(ncout, east, eArr)
ncvar_put(ncout, nrth, nArr)
# put additional attributes into dimension and data variables
ncatt_put(ncout,"east","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"nrth","axis","Y")
ncatt_put(ncout,"time","axis","T")
# add global attributes
ncatt_put(ncout,0,"title", 'MD-WERP, 9.2 Ecyhydrology Modelling')
ncatt_put(ncout,0,"institution", 'GRIFFITH UNIVERSITY')
ncatt_put(ncout,0,"source", 'SYNTHETIC')
# Close file
nc_close(ncout)
# }
