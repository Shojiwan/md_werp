# MD-WERP 9.2 Water Quality Modelling; Tuflow FV WQ 
# Ryan Shojinaga & David Hamilton, Australian Rivers Institute, Griffith Uni.
# ryan.shojinaga@griffithuni.edu.au

# Script to creates monthly Heatsource control files and batch run file

library(rstudioapi); library(lubridate)
remove(list = ls()); cat("\014")
# Get absolute path to model folder, load functions, and set input/output paths
path         <- getSourceEditorContext()$path
back_slash   <- unlist(gregexpr('/', path))
path         <- substr(path, 1, back_slash[length(back_slash) - 1])
source(paste0(path, '/003_R/999_model_functions.R'))
input_dir    <- paste0(path, '001_HS/in/')
output_dir   <- paste0(path, '001_HS/out/')
# Read in the control file (i.e., model start and end dates)
control_file <- readLines(paste0(path, 'culgoa_cf.mst'))
# Read in HS control file template and replace input/output paths
cf_template  <- readLines(paste0(path, "001_HS/cf/hs_cf_tmpl.csv"))
cf_template  <- gsub('XX_INPUT_DIR_XX', as.character(input_dir), cf_template)
cf_template  <- gsub('XX_OUTPUT_DIR_XX', as.character(output_dir), cf_template)
# Create the monthly control files (model is run in monthly batches)
start_date   <- clean_input(control_file[grep('start_date', control_file)])
end_date     <- clean_input(control_file[grep('end_date',   control_file)])
str_month    <- month(start_date)
end_month    <- month(end_date)
for (month in str_month : end_month) {
  first_date <- convert_2_date(year(start_date), month, 1)
  last_date  <- convert_2_date(year(start_date), month, 
                               days_in_month(first_date))
  first_date <- format(first_date, '%m/%d/%Y')
  last_date  <- format(last_date,  '%m/%d/%Y')
  hs_control <- gsub('XX_STRDATE_XX', first_date, cf_template)
  hs_control <- gsub('XX_ENDDATE_XX',  last_date,  hs_control)
  file_name  <- paste0('HS_cf_', ifelse(month < 10, '0', ''), month)
  writeLines(con = paste0(path, '001_HS/cf/', file_name, '.csv'),
             text = hs_control)
  # Create the run_heatsource batchfile
  batch_txt  <- c('',
                  # Copy from 001_HS/cf to 001_HS
                  paste0('copy   cf\\', file_name, '.csv ', 
                         'HeatSource_Control.csv'),
                  # Run heatsource
                  'python HS9_Run_Solar_Only.py',
                  # Delete control file
                  'delete HeatSource_Control.csv',
                  # Rename outputs
                  paste0('rename ', 'out\\Heat_SR1.csv Heat_SR1_', 
                         ifelse(month < 10, '0', ''), month, '.csv'),
                  paste0('rename ', 'out\\Heat_SR4.csv Heat_SR4_', 
                         ifelse(month < 10, '0', ''), month, '.csv'),
                  'cd ..')
  if (month == str_month) {
    batch_file <- batch_txt
  } else {
    batch_file <- append(batch_file, batch_txt)
  }
}
writeLines(text = batch_file, con = paste0(path, '001_HS/run_heatsource.bat'))
