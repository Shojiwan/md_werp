library(ggplot2); library(reshape2)
rm(list = ls()); cat("\014")
# dir1 <- 'F:/Backup/005_model/001_SLZ/01_inputs/03_hs'
# dir2 <- 'F:/003_gis/002_projData/008_MD/014_model/mesh'
dir3 <- 'F:/003_gis/001_allData/006_elevation/MD/md_werp/data'
# hdr1 <- names(read.csv(paste0(dir1, '/morphology.csv')))
# hdr2 <- names(read.csv(paste0(dir1, '/lcdata.csv')))
# data <- read.csv(paste0(dir2, '/culgoa_TW1_pnts.csv'))
xsct <- readRDS(paste0(dir3, '/xsct_trimmed.RData'))
bnks <- readRDS(paste0(dir3, '/stream_banks.RData'))
# Adjust cross sections
xsct$type <- 'grnd'
xsct <- rbind(xsct, bnks)
xsct <- xsct[order(xsct$cDst, xsct$tDst), ]
stns <- unique(xsct$stnX)
i = which(stns == 'STN04925')
cntr <- 1
for (i in 1 : length(stns)) {
  tmpX <- xsct[which(xsct$stnX == stns[i]), ]
  if (any(tmpX$type == 'LB01')) {
    iLB  <- which(tmpX$type == 'LB01'); iRB  <- which(tmpX$type == 'RB01')
    tmpX <- tmpX[iLB : iRB, ]
    # Isolate xscts that have a complete xsct w/i BNKF from those that don't
    if (!any(is.na(tmpX$grnd))) { # If the cross section has no NAs (complete)
      # Percentage of bankfull top width
      wTop <- tmpX$tDst[nrow(tmpX)] - tmpX$tDst[1]
      tmpX$pDst <- (tmpX$tDst - tmpX$tDst[1]) / wTop
      # Percentage of bankfull (max) depth
      dBkf <- tmpX$grnd[1] - min(tmpX$grnd)
      tmpX$pDpt <- 1 - (tmpX$grnd[1] - tmpX$grnd) / dBkf
      if (cntr == 1) {
        xscG <- tmpX[, c(9, 12, 13)]
        bkfG <- data.frame(stnX = stns[i], wTop = wTop, dBkf = dBkf)
      } else {
        xscG <- rbind(xscG, tmpX[, c(9, 12, 13)])
        bkfG <- rbind(bkfG, data.frame(stnX = stns[i], wTop = wTop, dBkf = dBkf))
      }
      cntr <- cntr + 1
    }
  }
}
# 1107 records of complete cross sections
# hist(xscG$pDst) # Ends are disproportionally represented, assume end points are 0
ends <- xscG[which(xscG$pDpt == 1), ]
xscG <- xscG[-which(xscG$pDpt == 1), ]
# Now bin on increments of 5%
xscG$bin <- ceiling(xscG$pDst * 100) / 100
# windows(12, 12)
# ggplot(data = xscG, aes(x = bin, y = pDpt, group = bin)) + geom_boxplot()
# So regress d[i + 1] ~ d[i] for i = 0.05, 0.10, 0.15, ..., 1.00
bins <- unique(xscG$bin)
bins <- bins[order(bins)]
i = 1
for (i in 1 : (length(bins) - 1)) {
  tmpX <- xscG[which(xscG$bin %in% c(bins[i], bins[i + 1])), ]
  tmpX <- dcast(tmpX, stnX ~ bin, value.var = 'pDpt', fun.aggregate = mean)
  regr <- summary(lm(tmpX[], ))
}












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

