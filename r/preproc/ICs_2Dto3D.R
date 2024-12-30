library(raster); library(ncdf4); library(lubridate); library(reshape2)
library(gsw); library(ggplot2)

remove(list = ls()); cat("\014")

# Use to 2D output (.nc) to populate ICs for 3D for the following:
# wl, temp, and sal
# USe HPH temp data to create vertical profile but use FV 2D temperatures
# Use HPH DO data to create global DO with vertical profile
# All other variables use global scalars with no stratification
# CHANGE THIS BASED ON WHERE THE 2D .nc FILE IS ________________________________ 
dir1 <- 'D:/Backup/005_model/008_MD/03_models/002_tuflow/culgoa_503/'
# CHANGE THIS BASED ON WHERE THE 2D .nc FILE IS ________________________________ 
dir2 <- "D:/Backup/004_data/002_projData/008_MD/from_UWA/MD WERP Data/"
dir3 <- 'D:/Backup/004_data/002_projData/008_MD/hydrology/flow_data'
dir4 <- 'D:/Backup/005_model/008_MD/03_models/002_tuflow/culgoa_032/'
tz   <- 'Australia/Brisbane'
#

# DATE OF MODEL START (including warm-up) ----
# Isolate index of starting time step: 2019-08-28 00:00. August has 744 hrly TSs 
indx <- data.frame(indx = 1 : 744, date = as.POSIXct(((1 : 744) - 1) * 3600,
                                                     origin = '2019-08-01',
                                                     tz = tz) - hours(10))
strD <- as.POSIXct('2019-08-28 00:00', '%Y-%m-%d %H:%M', tz = tz)
indx <- indx$indx[which(indx$date == strD)]
# ----

# Create IC table ----
# For 8 sigma layers, use this mapping: vertical cells ordered top to bottom
ics  <- read.csv(paste0(dir1, 'mapping_2D3D.csv'))[1 : 2]
hdrs <- read.csv(paste0(dir1, 'ICs_headers.csv'))
# ----

# Water level (spatially explicit; from Tulfow 2D)----
file <- nc_open(paste0(dir1, 'tfv_cntrl_file_08_aug_master.nc'))
zb   <- as.numeric(ncvar_get(file, 'cell_Zb'))
dpth <- ncvar_get(file, 'D')
wl   <- data.frame(ID2 = 1 : nrow(dpth), wl = zb + dpth[, indx])
wl$WD <- ifelse(round(wl$wl, 3) == round(zb, 3), 0, 1)
ics  <- merge(ics, wl, by.x = 'IDX2', by.y = 'ID2', all = T)
# ----

# Salinity (global; NSW real-time flow data) ----
# Set global salinity to converted conductivity when modelling period starts
saln <- read.csv(paste0(dir3, '/qDat_US_DS.csv'))
saln <- saln[which(saln$parm %in% c('c_uS', 't_C')), ]
saln$date <- as.POSIXct(saln$date, '%Y-%m-%d %H:%M:%S', tz = 'Australia/Brisbane') 
saln <- saln[which(saln$date == strD), ]
saln <- gsw_SP_from_C(saln$valu[which(saln$parm == 'c_uS')] / 1000, 
                      saln$valu[which(saln$parm == 't_C')], p = 10)
ics$sal <- saln
# ----

# Remaining WQ variables (global; from QLD WMIP) ----
# USing relationships developed in proc_wq_bcs.R
wq   <- read.csv(paste0(dir1, 'culgoa_007_US_wq.csv'))
wq$date <- as.POSIXct(wq$date, '%d/%m/%Y %H:%M', tz = tz)
wq   <- wq[which(wq$date == strD), ]

# ----

# DO (global w/ vert. prof; HPH) and temperature (spatial w/ vert prof; HPH) ---- 
# Process HPH data This section is a mess (tidy up later?)
data <- read.csv(paste0(dir2, 'Brenda_botNA_RMS.csv'))[c(1, 10 : 27, 29)]
dots <- gregexpr('\\.\\.', names(data))
for (i in 2 : (length(data) - 1)) {
  temp <- substr(names(data)[i], dots[[i]][1] + 2, nchar(names(data)[i]))
  temp <- as.numeric(gsub('\\.m\\.', '', temp))
  if (i == 2) {dpth <- temp} else {dpth <- append(dpth, temp)}
}
names(data) <- c('date', paste0('DOC', 1 : 8), paste0('TMP', 1 : 10), 'dpth')
dpth <- data.frame(indx = 1 : length(names(data)), name = names(data),
                   dpth = c(NA, dpth, NA))
dpth <- dpth[complete.cases(dpth$dpth), ]
data <- melt(data, id.vars = 'date', variable.name = 'parm', value.name = 'valu')
data <- merge(data, dpth, by.x = 'parm', by.y = 'name', all = T)
data$date <- as.POSIXct(data$date, '%d/%m/%Y %H:%M', tz = tz)
# min(data$date) # 2019-08-27 14:20:00 --> start at 2019-08-28 00:00
strD <- as.POSIXct('2019-08-28 00:00', '%Y-%m-%d %H:%M', tz = tz)
prfl <- data[which(data$date == strD), ]
dpth <- prfl$valu[which(is.na(prfl$indx))]
prfl <- prfl[order(prfl$indx), ]
prfl <- prfl[complete.cases(prfl$valu, prfl$indx), ]
cndD <- grep('DOC', prfl$parm); cndT <- grep('TMP', prfl$parm)
mean_DO <- mean(prfl$valu[cndD])
mean_TMP <- mean(prfl$valu[cndT])
add1 <- prfl[1, ]; add1$dpth <- 0 
add2 <- prfl[max(cndD), ]; add2$dpth <- dpth
add3 <- prfl[min(cndT), ]; add3$dpth <- 0 
add4 <- prfl[max(cndT), ]; add4$dpth <- dpth
prfl <- rbind(prfl, add1, add2, add3, add4)
prfl$parm <- substr(prfl$parm, 1, 3)
prfl <- prfl[order(prfl$parm, prfl$dpth), ]
prfl$sclr <- prfl$dPct <- NA
cndD <- grep('DOC', prfl$parm); cndT <- grep('TMP', prfl$parm)
prfl$dPct[cndD] <- prfl$dpth[cndD] / dpth
prfl$dPct[cndT] <- prfl$dpth[cndT] / dpth
prfl$sclr[cndD] <- prfl$valu[cndD] / mean_DO
prfl$sclr[cndT] <- prfl$valu[cndT] / mean_TMP
# Sigma profile should be center elevation of each of the 8 cells, hence, 
# halfway between the percentile breaks.
pSgm <- data.frame(pSgm = (1 : 8) / 8 - (1 / 8 / 2), TMP = NA, DOC = NA)
tmpD <- prfl[cndD, ]; tmpT <- prfl[cndT, ]
for (i in 1 : nrow(pSgm)) {
  # Start with temperature take difference in percentage of depth
  dffT <- tmpT$dPct - pSgm$pSgm[i]
  # Take the indeces that go from negative to positive
  dffT <- c(max(which(dffT < 0)), min(which(dffT >= 0)))
  # Interpolate
  pSgm$TMP[i] <- tmpT$sclr[dffT[1]] + (tmpT$sclr[dffT[2]] - tmpT$sclr[dffT[1]]) *
                                      (pSgm$pSgm[i] - tmpT$dPct[dffT[1]]) / 
                                      (tmpT$dPct[dffT[2]] - tmpT$dPct[dffT[1]])
  # Now DO
  dffD <- tmpD$dPct - pSgm$pSgm[i]
  dffD <- c(max(which(dffD < 0)), min(which(dffD >= 0)))
  pSgm$DOC[i] <- tmpD$sclr[dffD[1]] + (tmpD$sclr[dffD[2]] - tmpD$sclr[dffD[1]]) *
                                      (pSgm$pSgm[i] - tmpD$dPct[dffD[1]]) / 
                                      (tmpD$dPct[dffD[2]] - tmpD$dPct[dffD[1]])
}
# Create temperature first
temp <- ncvar_get(file, 'TEMP')
temp <- data.frame(ID2 = 1 : nrow(temp), temp = temp[, indx])
for (i in 1 : nrow(pSgm)) { # 1 = water surface; 8 = bottom of water column 
  temp$x <- temp$temp * pSgm$TMP[i] 
  names(temp)[i + 2] <- paste0('lvl', i)
}
temp <- melt(temp[, c(1, 3 : 10)], id.vars = 'ID2', value.name = 'temp',
             variable.name = 'LVL')
temp <- temp[order(temp$ID2, temp$LVL), ]
ics$temp <- temp$temp

# Populate the TSS (before DO)
ics$tss <- wq$FineSed_mgL.1

# Now populate 3D cells with the DO profile
pSgm$DOCV <- pSgm$DOC * mean_DO
ics$do <- pSgm$DOCV

# ----

# Go back to the remaining WQ parms and populate the IC table ----
parF <- tolower(names(wq))
parI <- tolower(hdrs$id[6 : nrow(hdrs)])
for (i in 1 : length(parI)) {
  ncol <- length(ics)
  colF <- which(parF == parI[i])
  ics$x <- ifelse(ics$WD == 0, 0, wq[, colF])
  names(ics)[ncol + 1] <- parI[i]
}
# zero out dry cells for sal, tss and do
for (i in c(5, 7, 8)) {
  ics[, i] <- ifelse(ics$WD == 0, 0, ics[, i])
}

# ----

# Okay delete interim columns and prit to csv
ics <- ics[, -c(1, 4)]
write.csv(x = ics, paste0(dir1, 'tfv_init_cond_2.csv'), row.names = F,
          quote = F)

# For 10 simga layers
# 2D -> 3D scheme. E.g.,      1 -     10 (ID3) =     1 (ID2)
# 2D -> 3D scheme. E.g.,  23321 -  23330 (ID3) =  2333 (ID2)
# 2D -> 3D scheme. E.g., 207611 - 207620 (ID3) = 20762 (ID2)



