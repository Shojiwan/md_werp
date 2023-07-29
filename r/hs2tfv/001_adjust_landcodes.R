# MD-WERP 9.2 Water Quality Modelling; Tuflow FV WQ 
# Ryan Shojinaga & David Hamilton, Australian Rivers Institute, Griffith Uni.
# ryan.shojinaga@griffithuni.edu.au

# Script to adjust the vegetation densities for creating short wave nc inputs

library(rstudioapi)
remove(list = ls()); cat("\014")
# Get absolute path to model folder and source functions
path         <- getSourceEditorContext()$path
back_slash   <- unlist(gregexpr('/', path))
path         <- substr(path, 1, back_slash[length(back_slash) - 1])
source(paste0(path, '/003_R/999_model_functions.R'))
# Read in the control file (i.e., density adjustment factor)
control_file <- readLines(paste0(path, 'culgoa_cf.mst'))
veg_dens_adj <- clean_input(control_file[grep('veg_dens_adj', control_file)])
# Read in the land cover codes and vegetation characteristic data
land_codes   <- read.csv(paste0(path, '001_HS/in/hs_in_lccodes_TMPL.csv'))
# Adjust vegetation density -- this takes fraction of difference between 100%
# density and the estimated base density (%) and adds that to the base density.
# For example if density == 40% and the adjustment factor is 2, the adjustment
# is half of difference of 100% and 40% ==> 60% / 2 = 30%. That gets added to 
# the 40% which is 70%. That's confusing, I'm sure, but I'll explain better
# later # Lo number = denser/more shade; Hi number = less dense/shade
cond <- which(land_codes$CANOPY != 0)
land_codes$CANOPY[cond] <- land_codes$CANOPY[cond] + 
                           (1 - land_codes$CANOPY[cond]) / 
                           veg_dens_adj
write.csv(file = paste0(dir2, '/hs_in_lccodes.csv'), row.names = F, 
          x = land_codes)