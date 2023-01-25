library(ggplot2); library(reshape2)
rm(list = ls()); cat("\014")
dir3 <- 'F:/003_gis/001_allData/006_elevation/MD/md_werp/data'
# dir3 <- 'D:/003_gis/001_allData/006_elevation/MD/md_werp/data'
xsct <- readRDS(paste0(dir3, '/xsct_trimmed.RData'))
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
xsct$type <- 'grnd'
xsct <- rbind(xsct, bnks)
xsct <- xsct[order(xsct$cDst, xsct$tDst), ]
stns <- unique(xsct$stnX)
# i = which(stns == 'STN03450')
cntr <- 1
for (i in 1 : length(stns)) {
  tmpX <- xsct[which(xsct$stnX == stns[i]), ]
  if (any(tmpX$type == 'LB01')) {
    iLB  <- which(tmpX$type == 'LB01'); iRB  <- which(tmpX$type == 'RB01')
    # Trim to bankfull dimensions and reposition LB to 0
    tmpX <- tmpX[iLB : iRB, ]
    tmpX$tDst <- tmpX$tDst - min(tmpX$tDst)
    wTop <- tmpX$tDst[nrow(tmpX)] - tmpX$tDst[1]
    dBkf <- max(tmpX$grnd) - min(tmpX$grnd)
    # Need to deal with dulplicate distances at the ends
    if (tmpX$tDst[1] == tmpX$tDst[2]) {
      tmpX <- tmpX[-2, ]
    }
    if (tmpX$tDst[nrow(tmpX) - 1] == tmpX$tDst[nrow(tmpX)]) {
      tmpX <- tmpX[-(nrow(tmpX) - 1), ]
    }
    if (!any(is.na(tmpX$grnd))) { # If the cross section has no NAs (complete)
      tmpG <- regularXS(tmpX = tmpX, nDiv = 50, xCol = 'tDst', zCol = 'grnd', 
                        dBkf = max(tmpX$grnd) - min(tmpX$grnd))
      tmpG$stnG <- stns[i]
      if (cntr == 1) {
        xscG <- tmpG
        bkfG <- data.frame(stnX = stns[i], wTop = wTop, dBkf = dBkf)
      } else {
        xscG <- rbind(xscG, tmpG)
        bkfG <- rbind(bkfG, data.frame(stnX = stns[i], wTop = wTop, dBkf = dBkf))
      }
      cntr <- cntr + 1
    }
  }
}
# 1107 records of complete cross sections
# Plot
# ggplot(data = xscG, aes(x = pInt, y = dInt, group = pInt)) + geom_boxplot()
# So regress d[i + 1] ~ d[i] for i = 0.02, 0.04, 0.06, ..., 0.96
# This method will rely on the first step from d = 0 to d = 0.02
# ----

# Create interval relationships ----
# NEED TO RE-EXAMINE
stnG <- unique(xscG$stnG)
bins <- unique(xscG$prct)
for (i in 2 : (length(bins) - 2)) {
  tmpX <- xscG[which(xscG$prct %in% c(bins[i], bins[i + 1])), ]
  tmpX <- dcast(tmpX, stnG ~ pInt, value.var = 'dInt', fun.aggregate = mean)
  tmp1 <- summary(lm(tmpX[, 3] ~ tmpX[, 2]))
  # Remove intercept if not significant
  if (tmp1[[4]][1, 4] > 0.05) tmp1 <- summary(lm(tmpX[, 3] ~ 0 + tmpX[, 2]))
  tmp2 <- data.frame(incr = paste0('P', names(tmpX)[2], '_P', names(tmpX)[3]),
                     slpe = ifelse(nrow(tmp1[[4]]) == 2, tmp1[[4]][2, 1], tmp1[[4]][1, 1]),
                     pSlp = ifelse(nrow(tmp1[[4]]) == 2, tmp1[[4]][2, 4], tmp1[[4]][1, 4]),
                     intc = ifelse(nrow(tmp1[[4]]) == 2, tmp1[[4]][1, 1], 0), 
                     pInt = ifelse(nrow(tmp1[[4]]) == 2, tmp1[[4]][1, 4], NA),
                     r2   = tmp1[['r.squared']],
                     se   = sqrt(sum(tmp1[['residuals']]^2) / tmp1[['df']][2]))
  if (i == 2) {regr <- tmp2} else {regr <- rbind(regr, tmp2)}
}
# Cool, these look good. Some of the intercepts are not significant, remove
# ----

# Test method ----
# Now test them against all of the data
# For each intersection, start at the left bank (BF; if it exists) and create:
# 1) Full cross section from LB to RB
# 2) Filled cross section using the ends of each gap as starting point
# For each xsct, select the depth that minimizes error of overall predictions

# Find maximum top width
dBkf <- seq(0.1, 15, 0.1)

# Will need to interpolate precentages of top width and BF depth 
stns <- unique(xsct$stnX) # 2828 total cross sections
i = which(stns == 'STN01275')
# i = which(stns == 'STN03450')
for (i in 1 : stns) {
  tmpX <- xsct[which(xsct$stnX == stns[i]), ]
  iLB  <- which(tmpX$type == 'LB01'); iRB  <- which(tmpX$type == 'RB01')
  # Trim to bankfull dimensions and reposition LB to 0
  tmpX <- tmpX[iLB : iRB, ]
  tmpX$tDst <- tmpX$tDst - min(tmpX$tDst)
  # Need to deal with dulplicate distances at the ends
  if (tmpX$tDst[1] == tmpX$tDst[2]) {
    tmpX <- tmpX[-2, ]
  }
  if (tmpX$tDst[nrow(tmpX) - 1] == tmpX$tDst[nrow(tmpX)]) {
    tmpX <- tmpX[-(nrow(tmpX) - 1), ]
  }
  # Top width and bankfull depth
  wTop <- tmpX$tDst[nrow(tmpX)] - tmpX$tDst[1]

  # 1) Full cross section
  dMin <- max(tmpX$grnd, na.rm = T) - min(tmpX$grnd, na.rm = T)
  tmpD <- dBkf # [which(dBkf > dMin)]
  j = 1
  for (j in 1 : length(tmpD)) {
    tmpG <- regularXS(tmpX = tmpX, nDiv = 50, xCol = 'tDst', zCol = 'grnd', 
                      dBkf = tmpD[j])
    tmpG$d2   <- NA
    indx <- c(1 : 2, nrow(tmpG) - 1, nrow(tmpG))
    tmpG$d2[indx] <- tmpG$dInt[indx]
    for (k in 1 : nrow(regr)) { # Generate the depth percentages
      tmpG$d2[k + 2] <- regr$slpe[k] * tmpG$d2[k + 1] + regr$intc[k]
    }
    tmpG$z2 <- tmpG$zInt[1] - tmpD[j] * tmpG$d2
    # Calculate standard error of residuals
    
    ggplot(tmpG, aes(x = wInt)) + geom_line(aes(y = zInt)) + geom_point(aes(y = z2))
    tmpE <- tmpG[-indx, ]
    tmpE$resd <- tmpE$zInt - tmpE$z2
    tmpE <- tmpE[complete.cases(tmpE$resd), ]
    err1 <- sqrt(sum(tmpE$resd^2) / (nrow(tmpE) - 2))
    tmp1 <- data.frame(stnG = stns[i], dpth = tmpD[j], eFul = err1)
    if (j == 1) {tmp2 <- tmp1} else {tmp2 <- rbind(tmp2, tmp1)}
  }

  plot(tmp2$eFul ~ tmp2$dpth)
  
  tmp2 <- tmp2[order(tmp2$eFul), ]; tmp2 <- tmp2[1, ]
  tmpG <- regularXS(tmpX = tmpX, nDiv = 50, xCol = 'tDst', zCol = 'grnd', 
                    dBkf = tmp2$dpth)
  tmpG$stnG <- stns[i]
  tmpG$d2   <- NA; indx <- c(1 : 2, nrow(tmpG) - 1, nrow(tmpG))
  tmpG$d2[indx] <- tmpG$dInt[indx]
  for (k in 1 : nrow(regr)) { # Generate the next depth percentage
    tmpG$d2[k + 2] <- regr$slpe[k] * tmpG$d2[k + 1] + regr$intc[k]
  }
  tmpG$z2 <- tmpG$zInt[1] - tmpD[j] * tmpG$d2
  ggplot() + geom_line(data = tmpX, aes(x = tDst, y = grnd), color = 'red') + 
    geom_line(data = tmpG, aes(x = wInt, y = z2), color = 'blue')
  
  
  # 2) Gap-filled cross section
  
  
  
}

# ----






# Bankfull depth estimates ----






# Deine the W/D relationships as porbability densities and run montecarlo on W
# windows(12, 12)
# ggplot(data = bkfG, aes(x = wTop, y = dBkf)) + geom_density_2d_filled(alpha = 0.8) + 
#   geom_point() + theme_bw() 







# ----












# Scratch ----
# hist(xscG$pDst) # Ends are disproportionally represented, assume end points are 0
ends <- xscG[which(xscG$pDpt == 1), ]
xscG <- xscG[-which(xscG$pDpt == 1), ]
# Now bin on increments of 5%
xscG$bin <- ceiling(xscG$pDst * 100) / 100

modl <- summary(lm(data = bkfG, dBkf ~ wTop)) # R2 = 0.2328
ggplot(data = bkfG, aes(x = wTop, y = dBkf)) + geom_point()


bkfG$wRut <- log10(bkfG$wTop); bkfG$dRut <- log10(bkfG$dBkf)
modl <- summary(lm(data = bkfG, dRut ~ wRut)) # R2 = 0.5956
line <- data.frame(x = c(min(bkfG$wRut), max(bkfG$wRut)),
                   y = modl[[4]][2, 1] * c(min(bkfG$wRut), max(bkfG$wRut)) + 
                       modl[[4]][1, 1])
ggplot(data = bkfG, aes(x = wRut, y = dRut)) + geom_point() +
  geom_line(data = line, aes(x = x, y = y))
# This line is close, though it will overpredict lower water levels

# Assume parabolic shape with no irregularities (e.g., bars, shelves, etc)

# rast <- readRDS(paste0(dir3, '/rasters.RData'))
# geom <- readRDS(paste0(dir3, '/geom_w_BF.RData'))

# dir1 <- 'F:/Backup/005_model/001_SLZ/01_inputs/03_hs'
# dir2 <- 'F:/003_gis/002_projData/008_MD/014_model/mesh'
# hdr1 <- names(read.csv(paste0(dir1, '/morphology.csv')))
# hdr2 <- names(read.csv(paste0(dir1, '/lcdata.csv')))
# data <- read.csv(paste0(dir2, '/culgoa_TW1_pnts.csv'))

# ggplot(data = bkfG, aes(x = wTop, y = dBkf)) + geom_density_2d_filled(alpha = 0.8) + 
#   geom_point() + theme_bw()
# Perhaps try a quadratic function - Nope
# expn <- seq(-5, 5, 0.1)
# rsqr <- slpe <- p_sl <- intc <- p_nt <- matrix(NA, nrow = length(expn), ncol = length(expn))
# for (i in 1 : length(expn)) {
#   for (j in 1 : length(expn)) {
#     test <- summary(lm(data = bkfG, I(dBkf^expn[i]) ~ I(wTop^expn[j])))
#     # rsqr[i, j] <- test[['r.squared']] # Rows (i) are depth, columns (j) are width
#     # slpe[i, j] <- ifelse(dim(test[[4]])[1] == 2, test[[4]][2, 1], NA)
#     # p_sl[i, j] <- ifelse(dim(test[[4]])[1] == 2, test[[4]][2, 4], NA)
#     # intc[i, j] <- ifelse(dim(test[[4]])[1] == 2, test[[4]][1, 1], NA)
#     # p_nt[i, j] <- ifelse(dim(test[[4]])[1] == 2, test[[4]][1, 4], NA)
#     temp <- data.frame(i = i, j = j, expW = expn[j], expD = expn[i], 
#                        slop = ifelse(dim(test[[4]])[1] == 2, test[[4]][2, 1], NA),
#                        p_sl = ifelse(dim(test[[4]])[1] == 2, test[[4]][2, 4], NA),
#                        intc = ifelse(dim(test[[4]])[1] == 2, test[[4]][1, 1], NA),
#                        p_nt = ifelse(dim(test[[4]])[1] == 2, test[[4]][1, 4], NA),
#                        r2 = test[['r.squared']])
#     if (i == 1 & j == 1) {rsq2 <- temp} else {rsq2 <- rbind(rsq2, temp)}
#   }
# }
# # library(raster); rsqr <- raster(summ); plot(rsqr)
# rsq2$indx <- 1 : nrow(rsq2)
# sigI <- rsq2[which(rsq2$p_nt <= 0.05), ]
# best <- rsq2[sigI$indx[which(sigI$r2 == max(sigI$r2))], ]
# line <- data.frame(x = line, 
#                    y = (best$slop * line^best$expW + best$intc)^(1 / best$expD))
# That is rubbish and this is pointless; use a straight line from origin
# expn <- 0.6 # I think this is probablr the 
# test <- summary(lm(data = bkfG, dBkf ~ 0 + I(wTop^expn)))
# line <- data.frame(x = seq(1, 151, 3), 
#                    y = test$coefficients[1, 1] * seq(1, 151, 3)^expn)

# ----