library(lidR); library(raster); library(reshape); library(ggplot2)
rm(list = ls()); cat("\014")
path <- 'F:/003_gis/001_allData/006_elevation/MD/point_cloud/culgoa'
laz <- readLAS(paste0(path, "/UpperDarlingLiDAR2014-C3-ELL_5406794_55_0001_0001.laz"))
head(laz@data)
plot(x = laz, legend = T)


tmpV <- filter_poi(laz, Classification %in% c(LASLOWVEGETATION,
                                              LASMEDIUMVEGETATION,
                                              LASHIGHVEGETATION))
tmpG <- filter_poi(laz, Classification %in% c(LASGROUND, LASWATER))
tmpV <- tmpV@data
tmpG <- tmpG@data
lims <- c(maxx = min(tmpV$X) + 100, maxy = min(tmpV$Y) + 100)
tmpV <- tmpV[which(tmpV$X < lims['maxx'] & tmpV$Y < lims['maxy']), ]
pl <- ggplot(tmpV, aes(x = X, y = Y)) + theme_minimal() +
      stat_density_2d(geom = "polygon", aes(fill = stat(level))) +
      # geom_point(data = tmpV, aes(x = X, y = Y)) +
      theme(axis.title = element_blank()); pl


# Take a slice at 540393-540394
tmpV <- tmpV[which(tmpV$X > 540393 & tmpV$X <  540394), ]
tmpG <- tmpG[which(tmpG$X > 540393 & tmpG$X <  540394), ]
# Aggregate ground points
tmpG$Y <- round(tmpG$Y, 0)
tmpG <- aggregate(tmpG$Z, by = list(tmpG$Y), FUN = 'mean')

ggplot() + geom_line(data = tmpG, aes(x = Group.1, y = x), size = 1.4, color = 'brown4') +
           stat_density_2d(data = tmpV, aes(x = Y, y = Z, fill = stat(level)),
                           geom = "polygon") + 
           geom_point(data = tmpV, aes(x = Y, y = Z)) +
           scale_x_continuous(limits = c(6794675, 6794825)) +


# plot(laz)
data <- read.csv(paste0(path, '/rstrs/vege_grnd.csv'))
for(i in 4 : 5) data[, i] <- as.numeric(data[, i])
data$VEGE <- ifelse(is.na(data$VEGE), data$GRND, data$VEGE)
names(data)[4 : 5] <- c('Vegetation', 'Ground')
data$DIST <- 0 : (nrow(data) - 1)
data <- melt(data[, c(1, 4, 5)], id.vars = 'DIST', value.name = 'z_m',
             variable_name = 'srce')
pl <- ggplot(data, aes(x = DIST, y = value, color = srce)) + 
      geom_line(size = 1.2) + 
      theme_classic() + theme(axis.line = element_blank(),
                              axis.title = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks = element_blank(),
                              legend.position = c(0.15, 0.75),
                              legend.title = element_blank(),
                              legend.text = element_text(size = 12),
                              axis.text.y = element_text(size = 12)) +
      scale_y_continuous(limits = c(160, 190), breaks = seq(160, 190, 5),
                         labels = paste0(seq(160, 190, 5), ' m')) +
      scale_color_manual(values = c('darkgreen', 'brown4')); pl
ggsave(filename = 'F:/Backup/001_griffith/md_werp/figures/vegetation_heights.png', 
       plot = pl, width = 41 * 0.55, height = 8, units = 'cm', dpi = 300)
max(laz@data$Z); min(laz@data$Z)

ndes <- read.csv("F:/003_gis/002_projData/008_MD/014_model/mesh/mesh_3_nodes.csv")
# ndes <- ndes[, c(2, 6, 7)]
rads <- read.csv('F:/003_gis/002_projData/008_MD/014_model/mesh/radial_points.csv')
i = 1
for (i in 1 : nrow(ndes)) {
  tmp1 <- ndes[i, ]
  tmp2 <- data.frame(node = ndes[i, 1], x = rads$x + tmp1$east, y = rads$y + tmp1$nrth)
  names(tmp1) <- names(tmp2)
  tmp2 <- rbind(tmp1, tmp2)
  if (i == 1) {tmp3 <- tmp2} else {tmp3 <- rbind(tmp3, tmp2)}
}
write.csv(x = tmp3, file = "F:/003_gis/002_projData/008_MD/014_model/mesh/mesh_3_rad_pnts.csv")
