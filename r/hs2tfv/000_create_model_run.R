library(rstudioapi)
remove(list = ls()); cat("\014")
# Get absolute path to model folder and source functions
path         <- getSourceEditorContext()$path
back_slash   <- unlist(gregexpr('/', path))
path         <- substr(path, 1, back_slash[length(back_slash) - 1])
source(paste0(path, '003_R/999_model_functions.R'))
# Read in the control file (component switches)
control_file <- readLines(paste0(path, 'culgoa_cf.mst'))
run_name     <- clean_input(control_file[grep('model_run_name', control_file)])
run_HS       <- clean_input(control_file[grep('run_heatsource', control_file)])
wind_bc      <- clean_input(control_file[grep('create_wind_bc', control_file)]) 
loss_bc      <- clean_input(control_file[grep('create_loss_bc', control_file)]) 
create_fvsed <- clean_input(control_file[grep('create_fvsed',   control_file)]) 
create_fvc   <- clean_input(control_file[grep('create_fvc',     control_file)]) 
# Read in batch file template
create_batch <- readLines(paste0(path, 'run_model.tmp'))
# Turn components on/off based on control file
if (run_HS == 0) {
  create_batch[grep('001_adjust_landcodes.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('001_adjust_landcodes.R', create_batch)])
  create_batch[grep('call heatsource.bat', create_batch)] <- 
    paste0('REM ', create_batch[grep('call heatsource.bat', create_batch)])
  create_batch[grep('Rscript 003_create_swr.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('Rscript 003_create_swr.R', create_batch)])
}
if (wind_bc == 0) {
  create_batch[grep('004_create_wind.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('004_create_wind.R', create_batch)])
}
if (loss_bc == 0) {
  create_batch[grep('005_create_loss.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('005_create_loss.R', create_batch)])
}
if (create_fvsed == 0) {
  create_batch[grep('006_create_fvsed.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('006_create_fvsed.R', create_batch)])
}
if (create_fvc == 0) {
  create_batch[grep('007_create_fvc.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('007_create_fvc.R', create_batch)])
}
create_batch[grep('TUFLOWFV', create_batch)] <- 
  paste0(create_batch[grep('TUFLOWFV', create_batch)], ' ', '002_TFV\\cf\\', 
         run_name, '.fvc')
writeLines(text = create_batch, con = paste0(path, 'run_model.bat'))
