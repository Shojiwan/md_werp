library(lidR); library(raster)
rm(list = ls()); cat("\014")
dir1 <- "F:/003_gis/001_allData/006_elevation/MD/point_cloud/culgoa/"
onCL <- read.csv(paste0(dir1, 'tils_OC_1km_40km.csv'))
proj <- shapefile(paste0(dir1, 'tiles.shp'))
proj <- proj@proj4string
tils <- list.files(dir1, pattern = '.laz')
cdeF <- substr(tils, 30, 36)
tils <- tils[which(cdeF %in% as.character(onCL$code))]
cdeF <- cdeF[which(cdeF %in% as.character(onCL$code))]
grps <- unique(onCL$grp)
cdeF <- data.frame(code = cdeF, file = tils)
i = j = 1
for (j in 1 : length(grps)) {
  tmpA <- onCL[which(onCL$grp == j), ]
  tmpA <- cdeF$file[which(cdeF$code %in% as.character(tmpA$code))]
  tmpT <- tmpA
  ctrV <- ctrW <- 1
  a <- Sys.time()
  for (i in 1 : length(tmpT)) {
    tmp1 <- readLAS(paste0(dir1, tmpT[i]))
    grnd <- filter_poi(tmp1, Classification %in% c(LASGROUND))
    vege <- filter_poi(tmp1, Classification %in% c(LASLOWVEGETATION,
                                                   LASMEDIUMVEGETATION,
                                                   LASHIGHVEGETATION))
    watr <- filter_poi(tmp1, Classification %in% c(LASWATER))
    tmp2 <- raster(xmn = min(tmp1@data$X), xmx = max(tmp1@data$X),
                   ymn = min(tmp1@data$Y), ymx = max(tmp1@data$Y), crs = proj,
                   ncols = 1000, nrows = 1000, vals = NULL)
    grnd <- rasterize(x = grnd@data[, 1 : 2], y = tmp2, field = grnd@data$Z,
                      fun = mean, na.rm = T)
    if (i == 1) {
      tmpG <- grnd
    } else {
      origin(grnd) <- origin(tmpG); tmpG <- merge(tmpG, grnd)
    }
    if (nrow(vege@data) != 0) {
      vege <- rasterize(x = vege@data[, 1 : 2], y = tmp2, field = vege@data$Z,
                        fun = mean, na.rm = T)
      if (ctrV == 1) {
        tmpV <- vege
        ctrV <- -1
      } else {
        origin(vege) <- origin(tmpV); tmpV <- merge(tmpV, vege)
      }
    }
    if (nrow(watr@data) != 0) {
      watr <- rasterize(x = watr@data[, 1 : 2], y = tmp2, field = watr@data$Z,
                        fun = mean, na.rm = T)
      if (ctrW == 1) {
        tmpW <- watr
        ctrW <- -1
      } else {
        origin(watr) <- origin(tmpW); tmpW <- merge(tmpW, watr)
      }
    }
  }
  writeRaster(x = tmpW, filename = paste0(dir1, 'rstrs/culgoa_watr_', j, '.asc'),
                overwrite = T)
  writeRaster(x = tmpG, filename = paste0(dir1, 'rstrs/culgoa_grnd_', j, '.asc'),
              overwrite = T)
  writeRaster(x = tmpV, filename = paste0(dir1, 'rstrs/culgoa_vege_', j, '.asc'),
              overwrite = T)
  b <- Sys.time()
}
# Last save was 11:53a
# Last save was 12:57p
# Last save was 13:37p
# Completed at  15:25p

# 
# 
# temp$xrnd <- round(temp$X, 0)
# tmp1 <- aggregate(temp$Z, by = list(temp$xrnd, temp$Y), FUN = 'mean')
# names(tmp1) <- c('east', 'nrth', 'z')
# temp$code <- substr(tils[i], 30, 36)
# temp <- temp@data[, 1 : 3]
# 
# 
# # temp <- raster(xmn = min(grnd@data$X), xmx = max(grnd@data$X),
# #                ymn = min(grnd@data$Y), ymx = max(grnd@data$Y), crs = data@crs,
# #                ncols = 1000, nrows = 1000, vals=NULL)
# # grn2 <- rasterize(x = grnd@data[, 1 : 2], y = temp, field = grnd@data$Z,
# #                   fun = mean, na.rm = T)c
# # writeRaster(x = grn2, filename = paste0(dir1, 'rstrs/grnd_UD14_', code, '.asc'))
# 
# fils <- list.files(paste0(dir1, "/rstrs"))
# grnF <- fils[grep('grnd', fils)]
# grnd <- list()
# temp <- shapefile(paste0(dir1, 'tiles.shp'))
# for (i in 1 : length(grnF)) grnd[[i]] <- raster(paste0(dir1, 'rstrs/', grnF[i]))
# for (i in 1 : length(grnF)) projection(grnd[[i]]) <- temp@proj4string
# for (i in 1 : length(grnF)) {
#   if (i == 1) {
#     tmpG <- grnd[[i]]
#   } else {
#     origin(grnd[[i]]) <- origin(tmpG)
#     tmpG <- merge(tmpG, grnd[[i]])
#   } 
# } 
# writeRaster(x = tmpG, filename = paste0(dir1, 'rstrs/grnd.asc'))


# fils <- list.files(dir1, pattern = '.laz')
# temp <- readLAS(paste0(dir1, fils[1]))
# onCL <- read.csv(paste0(dir1, 'tiles_on_culgoa.csv'))
# 
# for (i in 1 : nrow(onCL)) {
#   data <- readLAS(paste0(dir1, fils[i]))
#   code <- substr(fils[i], 30, 36)
#   grnd <- filter_poi(data, Classification %in% c(LASGROUND, LASWATER))
#   temp <- raster(xmn = min(grnd@data$X), xmx = max(grnd@data$X), 
#                  ymn = min(grnd@data$Y), ymx = max(grnd@data$Y), crs = data@crs, 
#                  ncols = 1000, nrows = 1000, vals=NULL)
#   grn2 <- rasterize(x = grnd@data[, 1 : 2], y = temp, field = grnd@data$Z, 
#                     fun = mean, na.rm = T)
#   writeRaster(x = grn2, filename = paste0(dir1, 'rstrs/grnd_UD14_', code, '.asc'))
#   watr <- filter_poi(data, Classification %in% c(LASWATER))
#   if (nrow(watr@data) != 0) {
#     wtr2 <- rasterize(x = watr@data[, 1 : 2], y = temp, field = watr@data$Z, 
#                       fun = mean, na.rm = T)
#     writeRaster(x = wtr2, filename = paste0(dir1, 'rstrs/watr_UD14_', code, '.asc'))
#   }
#   veg  <- filter_poi(data, Classification %in% c(LASLOWVEGETATION, LASMEDIUMVEGETATION,
#                                                  LASHIGHVEGETATION))
#   if (nrow(veg@data) != 0) {
#     veg2 <- rasterize(x = veg@data[, 1 : 2], y = temp, field = veg@data$Z, 
#                       fun = mean, na.rm = T)
#     writeRaster(x = veg2, filename = paste0(dir1, 'rstrs/veg_UD14_', code, '.asc'))
#   }
# }
  

# rm(list = ls()); cat("\014")
# # zip <- 4396986 # Southern (northern NSW)
# zip <- c(4396986, 4396921) # Southern (Culgoa in QLD)
# 
# for (i in 1 : 2) {
#   temp <- list.files(paste0('F:/003_gis/001_allData/006_elevation/MD/point_clo',
#                             'ud/DATA_', zip[i], '/Geoscience Australia/Point Cl',
#                             'ouds/Ellipsoidal'), pattern = '.zip')
#   temp <- data.frame(tile = temp, file = zip[i])
#   if (i == 1) {tils <- temp} else {tils <- rbind(tils, temp)} 
# }
# tilE <- unique(as.numeric(substr(tils$tile, 30, 32)) * 1000)
# tilN <- unique(as.numeric(substr(tils$tile, 33, 36)) * 1000)
# 
# for (i in 1 : length(tilE)) {
#   temp <- data.frame(EAST = tilE[i], NRTH = tilN)
#   if (i == 1) {tils <- temp} else {tils <- rbind(tils, temp)} 
# }
# # 
# # write.csv(file = paste0('F:/003_gis/001_allData/006_elevation/MD/point_cloud/',
# #                         'tiles.csv'), x = tils)
# 
# # File name coordinates are the lower left (southwest) tile corner
# # Read back in the tiles within the Culgoa 3K buffered area
# tils <- read.csv(paste0('F:/003_gis/001_allData/006_elevation/MD/point_cloud/',
#                         'tiles_culgoa.csv'))
# tils$file <- paste0('UpperDarlingLiDAR2014-C3-ELL_', tils$EAST / 1000, 
#                     tils$NRTH / 1000, '_55_0001_0001.zip')
# tmp1 <- list.files(paste0('F:/003_gis/001_allData/006_elevation/MD/point_clo',
#                           'ud/DATA_', zip[1], '/Geoscience Australia/Point Cl',
#                           'ouds/Ellipsoidal'), pattern = '.zip')
# tmp1 <- data.frame(tile = tmp1, file = zip[1])
# tmp2 <- list.files(paste0('F:/003_gis/001_allData/006_elevation/MD/point_clo',
#                           'ud/DATA_', zip[2], '/Geoscience Australia/Point Cl',
#                           'ouds/Ellipsoidal'), pattern = '.zip')
# tmp2 <- data.frame(tile = tmp2, file = zip[2])
# tils <- merge(tils, tmp1, by.x = 'file', by.y = 'tile', all.x = T, all.y = F)
# names(tils)[5] <- paste0('X', zip[1])
# tils <- merge(tils, tmp2, by.x = 'file', by.y = 'tile', all.x = T, all.y = F)
# NAs <- tils[which(is.na(tils$X4396986) & is.na(tils$X4396921)), ]
# names(tils)[6] <- paste0('X', zip[2])
# # Hmmm, pesky NAs. Check it out:
# # nas <- tils[which(is.na(tils$file.y)), ]
# # write.csv(x = nas, file = paste0('F:/003_gis/001_allData/006_elevation/MD/poin',
# #                                  't_cloud/nas.csv'))
# # Hmmm...I guess those files don't exist. Proceed, then...
# tils <- tils[-which(is.na(tils$X4396986) & is.na(tils$X4396921)), ] # 21 tils unaccounted for
# tils$fldr <- ifelse(is.na(tils$X4396986), tils$X4396921, tils$X4396986)
# 
# # F:\003_gis\001_allData\006_elevation\MD\point_cloud\DATA_4396986\Geoscience Australia\Point Clouds\Ellipsoidal
# 
# tils$pthF <- paste0('F:/003_gis/001_allData/006_elevation/MD/point_cloud/DATA_',
#                     tils$fldr, '/Geoscience Australia/Point Clouds/Ellipsoidal/',
#                     tils$file)
# tils$pthT <- paste0('F:/003_gis/001_allData/006_elevation/MD/point_cloud/culgoa/',
#                     'UDPC_2014_', tils$EAST / 1000, tils$NRTH / 1000, '.zip')
# file.exists(tils$pthF[100])
# tils$pthT[100]
# file.copy(tils$pthF, tils$pthT)
# # Now unzip and delete the zips
# library(zip)
# pth1 <- paste0('F:/003_gis/001_allData/006_elevation/MD/point_cloud/DATA_2384/',
#                'Geoscience Australia/Point Clouds/Ellipsoidal')
# fils <- list.files(pth1)
# for (i in 1 : length(fils)) {
#   unzip(zipfile = paste0(pth1, '/', fils[i]), overwrite = T,
#         exdir = 'F:/003_gis/001_allData/006_elevation/MD/point_cloud/culgoa/NAs')
# }
# file.exists(paste0(pth1, '/', fils[i]))
# file.exists(tils$pthT[i])
# x <- list.files()
# fil1 <- list.files(dir1, pattern = '.laz')
# fil2 <- list.files(paste0(dir1, 'NAs'), pattern = '.laz')
# fil2 <- fil2[which(fil1 %in% fil2)]
# fil2 <- fil2[complete.cases(fil2)]
# file.copy(paste0(dir1, 'NAs/', fil2), paste0(dir1, fil2))
# # copd <- c(T, T, T, T, F, T, T, F, T, T, F, T)
# # dncp <- ifelse(copd, F, T)
# # fil2[which(dncp)] # These didn't copy because they're already in there.
# fAll <- read.csv("F:/003_gis/001_allData/006_elevation/MD/point_cloud/tiles_culgoa.csv")
# fil1 <- list.files(dir1, pattern = '.laz') # 367 files - still missing 12!!
# fAll$code <- paste0(fAll$EAST / 1000, fAll$NRTH / 1000)
# fil1 <- substr(fil1, 30, 36)
# NAs <- fAll[which(!fAll$code %in% fil1), ]
# write.csv(x = NAs, file = 'F:/003_gis/001_allData/006_elevation/MD/point_cloud/NAs.csv')


# til <- list()
# df <- data.frame(matrix(NA, nrow = 0, ncol = 3))
# names(df) <- c('code', 'east', 'nrth')
# for (i in 1 : length(fils)) {
#   tmp1 <- data.frame(code = substr(fils[i], 30, 36), 
#                      east = as.numeric(substr(fils[i], 30, 32)) * 1000, 
#                      nrth = as.numeric(substr(fils[i], 33, 36)) * 1000)
#   til[[i]] <- Polygon(cbind(c(tmp1$east, tmp1$east, tmp1$east + 1000, tmp1$east + 1000, tmp1$east),
#                             c(tmp1$nrth, tmp1$nrth + 1000, tmp1$nrth + 1000, tmp1$nrth, tmp1$nrth)))
#   til[[i]] <- Polygons(list(til[[i]]), tmp1$code)
#   if (i == 1) {df <- tmp1} else {df <- rbind(df, tmp1)}
# }
# til = SpatialPolygons(Sr = til, proj4string = CRS(temp@crs$wkt))
# til = SpatialPolygonsDataFrame(Sr = til, data = df, match.ID = 'code')
# shapefile(til, paste0(dir1, 'tiles.shp'), overwrite = T)