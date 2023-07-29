library(raster); library(sf); library(ggplot2)
remove(list = ls()); cat("\014")
# dir1 <- 'D:/003_gis/002_projData/008_MD/014_model/mesh'
# dir2 <- 'D:/Backup/004_data/002_projData/004_MRY/geomorphology/cross_sections/'
dir1 <- 'F:/003_gis/002_projData/008_MD/014_model/mesh/'
dir2 <- 'F:/Backup/004_data/002_projData/004_MRY/geomorphology/cross_sections/'
source(paste0(dir2, "R/channel_geometry_functions.R"))
# CL <- shapefile(paste0(dir1, '/mesh_1_CL_pnts.shp'))
# CL <- shapefile(paste0(dir1, '/mesh_2_CL.shp'))
LB <- shapefile(paste0(dir1, '/mesh_1_LB.shp'))
# CL <- read.csv(paste0(dir1, 'culgoa_CL_pnts.csv'))
CL <- read.csv(paste0(dir1, 'culgoa_CL_pnts_w_side.csv'))
CL <- CL[-which(CL$id == 1), ]
# CL <- data.frame(CL@data, CL@coords)
CL <- CL[, c(1, 3, 2, 5)]; names(CL) <- c('sect', 'dist', 'east', 'nrth')
CL <- CL[order(CL$sect, CL$dist), ]
CL$midN <- CL$midE <- CL$aspc <- CL$cDst <- CL$tDst <- 0
# First point is a bogey--delete!
# CL <- CL[-1, ]
for (i in 2 : nrow(CL)) { # data define reach between point and previous point
  CL$tDst[i] <- sqrt((CL$nrth[i] - CL$nrth[i - 1])^2 +
                         (CL$east[i] - CL$east[i - 1])^2)
  CL$cDst[i] <- CL$cDst[i - 1] + CL$tDst[i]
  # Aspect defines the direction of the line from i - 1 to i; north = 0; east = 90, etc....
  east <- CL$east[i] - CL$east[i - 1]
  nrth <- CL$nrth[i] - CL$nrth[i - 1]
  CL$aspc[i] <- aspect(east, nrth)
  CL$midE[i] <- mean(c(CL$east[i], CL$east[i - 1]))
  CL$midN[i] <- mean(c(CL$nrth[i], CL$nrth[i - 1]))
}
pnts <- c(-75, 0, 75)
for (i in 2 : nrow(CL)) {
  orth <- orthog(angl = CL$aspc[i])
  coor <- data.frame(crk = CL$sect[i], cDst = CL$cDst[i],
                     dX = CL$midE[i] + orth[1] * pnts,
                     dY = CL$midN[i] + orth[2] * pnts)
  tmpM <- data.frame(crk = CL$sect[i], cDst = CL$cDst[i],
                     dX = CL$midE[i] + orth[1] * pnts[2],
                     dY = CL$midN[i] + orth[2] * pnts[2])
  if (i == 2) {
    xsct <- coor
    midP <- tmpM
  } else if (i > 2) {
    xsct <- rbind(xsct, coor)
    midP <- rbind(midP, tmpM)
  }
}
# midP <- midP[-c(1, nrow(midP)), c(2, 1, 3, 4)]
# midP <- midP[, c(2, 1, 3, 4)]
names(midP) <- c('tDst', 'sect', 'east', 'nrth')
midP$x <- 'MP'; midP$ID <- 1 : nrow(midP)
aDst <- unique(xsct$cDst)
tmp2 <- tmp3 <- list()
for (i in 1 : length(aDst)) {
  tmp1 <- xsct[which(xsct$cDst == aDst[i]), ]
  tmp2[[i]] <- Line(tmp1[, 3 : 4])
  tmp3[[i]] <- Lines(tmp2[[i]], ID = paste0(aDst[i], ' m'))
}
pLin <- SpatialLines(tmp3, proj4string = LB@proj4string)
# shapefile(filename = 'D:/003_gis/002_projData/008_MD/014_model/mesh/pLin.shp',
#           x = pLin, overwrite = T)
# shapefile(filename = 'f:/003_gis/002_projData/008_MD/014_model/mesh/culgoa_pLin.shp',
#           x = pLin, overwrite = T)
shapefile(filename = 'f:/003_gis/002_projData/008_MD/014_model/mesh/culgoa_pLin_side.shp',
          x = pLin, overwrite = T)
# write.csv(x = midP, file = paste0(dir1, '/midP_check.csv'))
# Read the RB/LB intersection points in (from QGIS)
# RB <- read.csv(paste0(dir1, '/mesh_2_RB_pnts.csv'))
# LB <- read.csv(paste0(dir1, '/mesh_2_LB_pnts.csv'))
bnks <- read.csv(paste0(dir1, '/culgoa_pLin_side_pnts.csv'))
# 1439 records - Shouldn't be odd number - Fix these
bnks <- bnks[, c(1, 4, 2, 5, 6)] # Don't need the "type" field no more
names(bnks) <- c('ID1', 'ID2', 'BNK', 'east', 'nrth')
bnks$ID3 <- paste0(bnks$ID1, "_", bnks$ID2)
tmpA <- aggregate(bnks$ID2, by = list(bnks$ID3), FUN = 'length')
tmpA <- data.frame(tmpA, ID1 = do.call("rbind", strsplit(tmpA$Group.1, "_"))[, 1],
                         ID2 = do.call("rbind", strsplit(tmpA$Group.1, "_"))[, 2])
for (i in 3 : 4) tmpA[, i] <- as.numeric(tmpA[, i])
names(tmpA)[1 : 2] <- c('ID3', 'sect')
# Find if any have no intersections
tmpA <- tmpA[order(tmpA$ID1, tmpA$ID2), ]
sect <- unique(tmpA$ID1)
tmpA$ID4 <- NA
for (i in sect) {
  cond <- which(tmpA$ID1 == i)
  tmpA$ID4[cond] <- 1 : length(cond)
}
none <- cbind(aggregate(tmpA$ID4, by = list(tmpA$ID1), FUN = 'length'),
              aggregate(tmpA$ID4, by = list(tmpA$ID1), FUN = 'max'))
# max and count are equal 
# Find which ones have an odd number of intersections
odds <- tmpA[which(tmpA$sect != 2), ]
# So the offending agent is Sect 14, ID2 482 intesects with Sect 15 - delete
bnks <- bnks[-which(bnks$ID3 == '15_482'), ]


# 5553 records, some lines must have more/less than 2 intersections. Check
# lbid <- bnks$ID[which(bnks$BNK == 'LB')]
# rbid <- bnks$ID[which(bnks$BNK == 'RB')]
# # lbid has 2777, rbid has 2776; but there are 2778 pLines altogether
# length(unique(lbid)); length(unique(rbid)) # lbid has 1 douple intersect
# bnks[which(bnks$ID == lbid[which(duplicated(lbid))]), ]
# 
# # Merge these with CL and check in QGIS
# pnts <- rbind(midP, RB, LB)
# Need to redo midP because many of the lines have changed
library(reshape2)
ids <- unique(bnks$ID3) # 720 records
i = 1
for (i in 1 : length(ids)) {
  ID1 <- as.numeric(strsplit(x = ids[i], split = "_")[[1]][1])
  ID2 <- as.numeric(strsplit(x = ids[i], split = "_")[[1]][2])
  tmpX <- bnks[which(bnks$ID3 == ids[i]), ]
  # tmpM <- data.frame(ID = ids[i], BNK = 'MP', east = mean(tmpX$east), 
  #                    nrth = mean(tmpX$nrth))
  tmpM <- data.frame(ID1 = ID1, ID2 = ID2, BNK = 'MP', east = mean(tmpX$east), 
                     nrth = mean(tmpX$nrth), ID3 = ids[i])
  bnks <- rbind(bnks, tmpM)
}
# bnks <- melt(bnks, id.vars = 'BNK', value.name = '', variable.name = )
# write.csv(x = pnts, file = paste0(dir1, '/pnts_chck.csv'))
# For the centerline, lop off first and last points (no intersections)
# Yup, all good.
# Now dissect each line segment into 3
# Create the quads to match the points. Go CL-RB, CL-LB, from top to bottom
# Lateral indeces: CL RB1 RB2 RB3 CL LB1 LB2 LB3
side <- c('RB', 'LB')
# DISSECT
x = 2; i = 10; j = 1
for (x in sect) {
  tmpB<- bnks[which(bnks$ID1 == x), ]
  sID  <- unique(tmpB$ID3)
  for (i in 1 : length(sID)) {
    tmpX <- tmpB[which(tmpB$ID3 == sID[i]), ]
    if (nrow(tmpX) != 0) {
      for (j in 1 : 2) {
        tmpS <- tmpX[which(tmpX$BNK %in% c(side[j], 'MP')), ]
        a <- which(tmpS$BNK == 'MP'); b <- which(tmpS$BNK == side[j])
        dX   <- (tmpS$east[b] - tmpS$east[a]) / 3 # from CL to bank
        dY   <- (tmpS$nrth[b] - tmpS$nrth[a]) / 3 # from CL to bank
        for (k in 1 : 2) {
          tmpA <- data.frame(ID1  = tmpS$ID1[which(tmpS$BNK == 'MP')],
                             ID2  = tmpS$ID2[which(tmpS$BNK == 'MP')],
                             BNK  = paste0(side[j], k),
                             east = tmpS$east[a] + k * dX,
                             nrth = tmpS$nrth[a] + k * dY,
                             ID3  = tmpS$ID3[which(tmpS$BNK == 'MP')])
          tmpX <- rbind(tmpX, tmpA)
        }
      }
      if (i == 1) {tmp2 <- tmpX} else {tmp2 <- rbind(tmp2, tmpX)}
    }
  }
  if (x == 2) {xsct <- tmp2} else {xsct <- rbind(xsct, tmp2)}
}

names(xsct)[2] <- c('pnt')
ordr <- data.frame(name = c('RB', 'RB2', 'RB1', 'MP', 'LB1', 'LB2', 'LB'), 
                   ordr = 1 : 7)
# xsct$ID <- ifelse(xsct$ID > 779, xsct$ID - 1, xsct$ID)
xsct <- merge(xsct, ordr, by.x = 'BNK', by.y = 'name', all = T)
xsct$BNK <- ifelse(xsct$BNK == 'RB', 'RB3',
                   ifelse(xsct$BNK == 'LB', 'LB3', xsct$BNK))
xsct <- xsct[order(xsct$ID1, xsct$ID2, xsct$ordr), ]
# Now create polygons 
cntr <- 1
tmp1 <- tmp2 <- list()
k = 2; i = 1; j = 1
for (k in sect) {
  tmpZ <- xsct[which(xsct$ID1 == k), ]
  ID3  <- unique(tmpZ$ID3)
  for (i in 1 : (length(ID3) - 1)) {
    tmpX <- tmpZ[which(tmpZ$ID3 %in% c(ID3[i], ID3[i + 1])), ]
    nCll <- max(tmpX$ordr) - 1
    for (j in 1 : nCll) {
      tmpQ <- tmpX[which(tmpX$ordr %in% c(j, j + 1)), ]
      lat1 <- min(tmpQ$ordr); lat2 <- max(tmpQ$ordr)
      lon1 <- min(tmpQ$ID2); lon2 <- max(tmpQ$ID2)
      ord1 <- which(tmpQ$ordr == lat1 & tmpQ$ID2 == lon1)
      ord2 <- which(tmpQ$ordr == lat1 & tmpQ$ID2 == lon2)
      ord3 <- which(tmpQ$ordr == lat2 & tmpQ$ID2 == lon2)
      ord4 <- which(tmpQ$ordr == lat2 & tmpQ$ID2 == lon1)
      x    <- c(tmpQ$east[ord1], 
                tmpQ$east[ord2], 
                tmpQ$east[ord3], 
                tmpQ$east[ord4], 
                tmpQ$east[ord1])
      y    <- c(tmpQ$nrth[ord1], 
                tmpQ$nrth[ord2], 
                tmpQ$nrth[ord3], 
                tmpQ$nrth[ord4], 
                tmpQ$nrth[ord1])
      tmp1[[cntr]] <- Polygon(cbind(x, y))
      tmp2[[cntr]] <- Polygons(list(tmp1[[cntr]]), ID = cntr)
      if (cntr == 1) {DF <- tmpQ[1, ]} else {DF <- rbind(DF, tmpQ[1, ])}
      cntr <- cntr + 1
    }
  }
}
mesh <- SpatialPolygonsDataFrame(Sr = tmp2, data = DF, 
                                 proj4string = pLin@proj4string)
shapefile(mesh, paste0(dir1, '/culgoa_test_2.shp'), overwrite = T)
# That looks pretty good. Proceed in GIS until you need th general mesh



# Import the test shape
msh3 <- shapefile(paste0(dir1, '/triangular_1.shp'))
coor <- msh3@polygons[[1]]@Polygons[[1]]@coords
# calculate the midpoints
for (i in 1 : (nrow(coor) - 1)) {
  dX <- (coor[i, 1] - coor[i + 1, 1]) / 2
  dY <- (coor[i, 2] - coor[i + 1, 2]) / 2
  temp <- c(coor[i, 1] - dX, coor[i, 2] - dY)
  if (i == 1) {
    midP <- temp
    dAvg <- c(abs(dX * 2), abs(dY * 2))
  } else {
    midP <- rbind(midP, temp)
    dAvg <- append(dAvg, c(abs(dX * 2), abs(dY * 2)))
  }
}
mean(dAvg) # Mean is 12.3m -- Use 25 m instead
x <- seq(min(coor[, 1]) - 25, max(coor[, 1]) + 25, 25)
y <- seq(min(coor[, 2]) - 25, max(coor[, 2]) + 25, 25)
for (i in 1 : length(x)) {
  temp <- data.frame(x = x[i], y = y)
  if (i == 1) {tPnt <- temp} else {tPnt <- rbind(tPnt, temp)}
}
midP <- data.frame(x = midP[, 1], y = midP[, 2])
tPnt <- rbind(tPnt, midP)
write.csv(tPnt, paste0(dir1, '/triangles_1_pnts.csv'))










