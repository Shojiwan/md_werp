# Use the methods from bankfull chapter: bench index and WD ratio
# First using the Australian Hydrological Geospatial Fabric (Geofabric) V3.2.1
# Using both stream segments and nodes
library(raster); library(ggplot2); library(reshape2)
library(grid); library(gridExtra)
rm(list = ls()); cat("\014")
dir1 <- 'F:/003_gis/001_allData/006_elevation/MD/md_werp/gis/'
dir2 <- 'F:/003_gis/001_allData/006_elevation/MD/point_cloud/culgoa/rstrs'
dir3 <- 'G:/004_data/002_projData/004_MRY/geomorphology/cross_sections/'
dir4 <- 'F:/003_gis/001_allData/006_elevation/MD/point_cloud/culgoa/xsct/'
source(paste0(dir3, "R/channel_geometry_functions.R"))

# X-section point locations ----
# Calculate x-section transect points - this next section is to create the cross 
# section points, and is only needed when that function needs to be redone 
# (e.g., main channel and off channel)
# pnts <- read.csv(paste0(dir1, 'culgoa_line.csv'))
# pnts <- pnts[, - (6 : 7)]
# pnts <- pnts[order(pnts$distance), ]
# names(pnts)[1 : 2] <- c('cDst', 'aspc')
# pnts$midN <- pnts$midE <- pnts$tDst <- 0
# for (i in 2 : nrow(pnts)) { # data define reach between point and previous point
#   pnts$tDst[i] <- sqrt((pnts$nrth[i] - pnts$nrth[i - 1])^2 +
#                          (pnts$east[i] - pnts$east[i - 1])^2)
#   # Aspect defines the direction of the line from i - 1 to i; north = 0; east = 90, etc....
#   east <- pnts$east[i] - pnts$east[i - 1]
#   nrth <- pnts$nrth[i] - pnts$nrth[i - 1]
#   pnts$aspc[i] <- aspect(east, nrth)
#   pnts$midE[i] <- mean(c(pnts$east[i], pnts$east[i - 1]))
#   pnts$midN[i] <- mean(c(pnts$nrth[i], pnts$nrth[i - 1]))
# }
# # @ each segment mid-point, take a 70m line perpendicular to the segment aspect
# trns <- seq(-70, 70, 2)
# for (i in 2 : nrow(pnts)) {
#   orth <- orthog(angl = pnts$aspc[i])
#   # for transect distance (tDst) negative refers to left bank, positive right bank
#   coor <- data.frame(cDst = pnts$cDst[i], tDst = trns,
#                      dX = pnts$midE[i] + orth[1] * trns,
#                      dY = pnts$midN[i] + orth[2] * trns)
#   if (i == 2) {xsct <- coor} else if (i > 2) {xsct <- rbind(xsct, coor)}
# }
# write.csv(x = xsct, file = paste0(dir1, 'xsct.csv'), row.names = F)
# ----

# Extract elevations ----
# xsct <- read.csv(paste0(dir1, 'xsct.csv'))
# ndes <- read.csv(paste0(dir1, 'profiles_25m.csv'))
# xsct <- merge(xsct, ndes[, c(1, 6)], by.x = 'cDst', by.y = 'cDst', all.x = T)
# grps <- unique(xsct$grp)
# fils <- list.files(path = dir2, pattern = '.asc')
# fils <- paste0(dir2, '/', fils)
# rstr <- list(grnd = list(), vege = list(), watr = list())
# for (i in 1 : 9) {
#   for (j in 1 : 3) {
#     rstr[[j]][[i]] <- raster(fils[grep(cmpt[j], fils)][i])
#   }
# }
# # Sample elevations at each point in the cross sections
# xsct$vege <- xsct$watr <- xsct$grnd <- NA
# for (i in 1 : length(grps)) {
#   cond <- which(xsct$grp == i)
#   temp <- xsct[cond, ]
#   for (j in 1 : length(cmpt)) {
#     xsct[cond, j + 5] <- raster::extract(x = rstr[[j]][[i]], y = temp[, 3 : 4])
#   }
# }
# # Oops, wrong names
# names(rstr)[2] <- 'watr'; names(rstr)[3] <- 'vege'
# names(xsct)[7 : 8] <- c('vege', 'watr')
# ----

# Save as interim data ----
dir5 <- 'F:/003_gis/001_allData/006_elevation/MD/md_werp'
# saveRDS(object = list(cross_sections = xsct, rasters = rstr, nodes = ndes), 
#         file = paste0(dir5, '/cross_section_data.RData'))
xsct <- readRDS(paste0(dir5, '/data/cross_section_data.RData'))[[1]]
rstr <- readRDS(paste0(dir5, '/data/cross_section_data.RData'))[[2]]
ndes <- readRDS(paste0(dir5, '/data/cross_section_data.RData'))[[3]]
stns <- unique(xsct$cDst)
cmpt <- c('grnd', 'vege', 'watr')
# ----

# Plot these up! ----
# pl <- list()
# for (i in 1 : length(stns)) {
#   temp <- xsct[which(xsct$cDst == stns[i]), ]
#   # Print 5 to a sheet (A4)
#   temp <- melt(data = temp[, c(2, 6 : 8)], id.var = 'tDst', value.name = 'z', 
#                variable.name = 'layr')
#   prfx <- ifelse(stns[i] < 100, '000', 
#                  ifelse(stns[i] < 1000, '00', 
#                         ifelse(stns[i] < 10000, '0','')))
#   lblY <- min(temp$z, na.rm = T) + 0.9 * (max(temp$z, na.rm = T) - min(temp$z, na.rm = T))
#   pl[[i]] <- ggplot(temp, aes(x = tDst, y = z, color = layr)) + geom_line(size = 1.5) +
#              scale_color_manual(values = c("brown", "darkgreen", "darkblue"),
#                                 labels = c("Ground", "Vegetation", "Water")) +
#              theme_bw() + theme(axis.title = element_blank(), 
#                                 legend.title = element_blank(),
#                                 legend.position = c(0.1, 0.7)) + 
#              annotate(geom = 'text', x = 0, y = lblY, size = 5,
#                       label = paste0('STN', prfx, stns[i]))
#   names(pl)[i] <- paste0('STN', prfx, stns[i])
# }
# npge <- ceiling(length(stns)/5)
# vctr <- 1 : 5
# for (i in 1 : npge) {
#   indx <- vctr + (i - 1) * 5
#   pX <- grid.arrange(pl[[indx[1]]], 
#                      pl[[indx[2]]], 
#                      pl[[indx[3]]], 
#                      pl[[indx[4]]],
#                      pl[[indx[5]]],
#                      ncol = 1)
#   name <- paste0(names(pl)[indx[1]], '_', names(pl)[indx[5]])
#   ggsave(filename = paste0(dir4, name, '.png'), plot = pX, width = 21, 
#          height = 29.7, units = "cm", dpi = 300)
# }
# These look pretty good. Now start the analysis (bankfull identification) ----

# Bankfull dimensions ----
# Calcalate channel dimensions and bankfull elevations ---- 
cntr <- 1
for (i in 1 : length(stns)) {
  temp <- xsct[which(xsct$cDst == stns[i]), ]
  temp <- temp[complete.cases(temp$grnd), ]
  if (nrow(temp) != 0) {
    temp$tDst <- temp$tDst - min(temp$tDst)
    # Get rid of down slopes on the edges; start at left bank
    ends <- rep(T, nrow(temp))
    for (k in 1 : (nrow(temp) - 1)) {
      if (temp$grnd[k] <= temp$grnd[k + 1]) {
        ends[k] <- FALSE
      } else {
        break 
      }
    }
    for (k in nrow(temp) : 2) {
      if (temp$grnd[k] <= temp$grnd[k - 1]) {
        ends[k] <- FALSE
      } else {
        break
      }
    }
    temp <- temp[which(ends), ]
    # Calculate channel geometry at 0.1m increments through the xsct from bottom 
    # to top. Then calculate WD and BI from these.
    zRng <- seq(floor(min(temp$grnd) * 10) / 10, ceiling(max(temp$grnd) * 10) / 10,
                0.1)
    for (j in 1 : length(zRng)) {
      if (zRng[j] > min(temp$grnd) & zRng[j] < temp$grnd[1] & zRng[j] < temp$grnd[nrow(temp)]) {
        # Now calculate the geometry
        aXsc <- getXsctArea(wse = zRng[j], xsct = temp, colX = 'tDst', colZ = 'grnd')
        wTop <- getTopWidth(wse = zRng[j], xsct = temp, colX = 'tDst', colZ = 'grnd')
        dAvg <- aXsc / wTop
        # Assume max depth is = minimum xsct elevation and wse 
        dMax <- zRng[j] - min(temp$grnd)
        tmp1 <- data.frame(stn = stns[i], 
                           wse = zRng[j], 
                           aXsc = aXsc, 
                           wTop = wTop,
                           dAvg = dAvg, 
                           dMax = dMax)
        if (cntr == 1) {
          tmp2 <- tmp1
        } else {
          tmp2 <- rbind(tmp2,
                        data.frame(stn = stns[i], 
                                   wse = zRng[j], 
                                   aXsc = aXsc,
                                   wTop = wTop, 
                                   dAvg = dAvg, 
                                   dMax = dMax))
        }
        cntr <- cntr + 1
      }
    }
  }
}
# So some of these transects were null for some reason. Investigate why...
which(unique())






















