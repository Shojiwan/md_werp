library(ggplot2); library(reshape2); library(grid); library(gridExtra)
rm(list = ls()); cat("\014")
dir3 <- 'F:/003_gis/001_allData/006_elevation/MD/md_werp/data'
xsct <- readRDS(paste0(dir3, '/xsct_trimmed.RData'))
xscG <- readRDS(paste0(dir3, '/xsct_generalized_complete.RData'))
bnks <- readRDS(paste0(dir3, '/stream_banks.RData'))

# FUNCTIONS ----
interpX <- function(x1, x2, y1, y2, xI) {
  yI <- y1 + (y2 - y1) * (xI - x1) / (x2 - x1)
  return(yI)
}
regularXS <- function(tmpX, nDiv, xCol, zCol, dBkf) {
  colX <- which(names(tmpX) == xCol); colZ <- which(names(tmpX) == zCol)
  wTop <- tmpX[nrow(tmpX), colX] - tmpX[1, colX]
  # Take (interpolated) depths at regular percentages of bankfull top width
  tmpG <- data.frame(prct = seq(0, 100, 100 / nDiv),
                     pInt = seq(0, 100, 100 / nDiv) / 100, 
                     wInt = seq(0, 100, 100 / nDiv) / 100 * wTop)
  tmpG$zInt <- NA # Distance at intervals
  tmpG$zInt[1] <- tmpX[1, colZ]
  for (j in 2 : nrow(tmpG)) {
    # Find indeces of tmpX that bracket the wInt[j]
    diff <- data.frame(indx = 1 : nrow(tmpX),
                       diff = abs(tmpG$wInt[j] - tmpX[, colX]))
    diff <- diff[order(diff$diff), ]
    # Plug those into to interpolate the elevation
    tmpG$zInt[j] <- interpX(x1 = tmpX[diff$indx[1], colX], 
                            x2 = tmpX[diff$indx[2], colX], 
                            y1 = tmpX[diff$indx[1], colZ], 
                            y2 = tmpX[diff$indx[2], colZ], 
                            xI = tmpG$wInt[j])
  }
  # Percentage of bankfull (max) depth
  tmpG$dInt <- (tmpG$zInt[1] - tmpG$zInt) / dBkf
  return(tmpG)
}

# ----

# Create cross sections of standardized W & D intervals ----
# Adjust cross sections
# xsct$type <- 'grnd'
# xsct <- rbind(xsct, bnks)
# xsct <- xsct[order(xsct$cDst, xsct$tDst), ]
# stns <- unique(xsct$stnX)
# i = which(stns == 'STN03450')
# cntr <- 1
# for (i in 1 : length(stns)) {
#   tmpX <- xsct[which(xsct$stnX == stns[i]), ]
#   if (any(tmpX$type == 'LB01')) {
#     iLB  <- which(tmpX$type == 'LB01'); iRB  <- which(tmpX$type == 'RB01')
#     # Trim to bankfull dimensions and reposition LB to 0
#     tmpX <- tmpX[iLB : iRB, ]
#     tmpX$tDst <- tmpX$tDst - min(tmpX$tDst)
#     wTop <- tmpX$tDst[nrow(tmpX)] - tmpX$tDst[1]
#     dBkf <- max(tmpX$grnd) - min(tmpX$grnd)
#     # Need to deal with dulplicate distances at the ends
#     if (tmpX$tDst[1] == tmpX$tDst[2]) {
#       tmpX <- tmpX[-2, ]
#     }
#     if (tmpX$tDst[nrow(tmpX) - 1] == tmpX$tDst[nrow(tmpX)]) {
#       tmpX <- tmpX[-(nrow(tmpX) - 1), ]
#     }
#     if (!any(is.na(tmpX$grnd))) { # If the cross section has no NAs (complete)
#       tmpG <- regularXS(tmpX = tmpX, nDiv = 50, xCol = 'tDst', zCol = 'grnd',
#                         dBkf = max(tmpX$grnd) - min(tmpX$grnd))
#       tmpG$stnG <- stns[i]
#       if (cntr == 1) {
#         xscG <- tmpG
#         bkfG <- data.frame(stnX = stns[i], wTop = wTop, dBkf = dBkf)
#       } else {
#         xscG <- rbind(xscG, tmpG)
#         bkfG <- rbind(bkfG, data.frame(stnX = stns[i], wTop = wTop, dBkf = dBkf))
#       }
#       cntr <- cntr + 1
#     }
#   }
# }
# saveRDS(object = xscG, file = paste0(dir3, '/xsct_generalized_complete.RData'))
# 1107 records of complete cross sections
# Plot
# ggplot(data = xscG, aes(x = pInt, y = dInt, group = pInt)) + geom_boxplot()
# So regress d[i + 1] ~ d[i] for i = 0.02, 0.04, 0.06, ..., 0.96
# This method will rely on the first step from d = 0 to d = 0.02
# ----

# Add area and wetted permiter to xscG. Mid-point trapezoids where ends 
# represent half areas; e.g., 0-1, 1-3, 3-5, 5-7, ..., 97-99, 99-100
stns <- unique(xscG$stnG)
# j <- which(stns == 'STN62425')
for (j in 1 : length(stns)) {
  tmpG <- xscG[which(xscG$stnG == stns[j]), ]
  wdth <- tmpG$wInt[2] - tmpG$wInt[1]
  wse  <- tmpG$zInt[1]
  tmpG$pWet <- tmpG$aInt <- NA
  for (i in 1 : nrow(tmpG)) {
    if (i == 1) { # The first sections
      d1 <- wse - tmpG$zInt[i + 1]
      tmpG$aInt[i] <- d1 / 2 * wdth / 2 # Area
      tmpG$pWet[i] <- sqrt((wdth / 2)^2 + d1^2)
    } else if (i == nrow(tmpG)) { # Last section
      d1 <- wse - tmpG$zInt[i - 1]
      tmpG$aInt[i] <- d1 / 2 * wdth / 2
      tmpG$pWet[i] <- sqrt((wdth / 2)^2 + d1^2)
    } else { # All of the middle sections
      d1 <- wse - (tmpG$zInt[i - 1] + tmpG$zInt[i]) / 2 # Mid point z of z(i-1) & z(i)
      d2 <- wse - tmpG$zInt[i]
      d3 <- wse - (tmpG$zInt[i] + tmpG$zInt[i + 1]) / 2 # Mid point z of z(i) & z(i+1)
      a1 <- (d1 + d2) / 2 * wdth / 2
      a2 <- (d2 + d3) / 2 * wdth / 2
      tmpG$aInt[i] <- a1 + a2
      p1 <- sqrt((wdth / 2)^2 + (d1 - d2)^2)
      p2 <- sqrt((wdth / 2)^2 + (d2 - d3)^2)
      tmpG$pWet[i] <- p1 + p2
    }
  }
  tmp1 <- data.frame(stn = stns[j], wTop = tmpG$wInt[nrow(tmpG)], 
                     dBkf = wse - min(tmpG$zInt), aXsc = sum(tmpG$aInt), 
                     dAvg = sum(tmpG$aInt) / tmpG$wInt[nrow(tmpG)], 
                     pWet = sum(tmpG$pWet))
  if (j == 1) {
    geom <- tmp1; tmp2 <- tmpG
  } else {
    geom <- rbind(geom, tmp1); tmp2 <- rbind(tmp2, tmpG)
  }
}
xscG <- tmp2
























