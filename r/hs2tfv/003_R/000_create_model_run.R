remove(list = ls()); cat("\014")
# Set absolute path to model folder
# FOLDER PATH HERE (Remember to replace '\' with '/')____________________________
# path <- MODEL_RUN_FOLDER_PATH
path <- 'C:/001_MD_WERP/001_Culgoa/002_TFV/culgoa_545'
# E.g., path <- 'C:/001_MD_WERP/001_Culgoa/002_TFV/culgoa_013_hcal/'
# Check path
if (dir.exists(path)) {
  last_char <- substr(path, nchar(path), nchar(path))
  if (last_char != '/') path <- paste0(path, '/')
} else {
  cat('File path does not exist.'); break
}
#_______________________________________________________________________________
source(paste0(path, '003_R/999_model_functions.R'))
# Write scripts with path
write_scripts(path)
# Read in the control file (component switches)
control_file <- readLines(paste0(path, 'culgoa_cf.mst'))
run_name     <- clean_input(control_file[grep('model_run_name',  control_file)])
run_HS       <- clean_input(control_file[grep('run_heatsource',  control_file)])
wind_bc      <- clean_input(control_file[grep('create_wind_bc',  control_file)]) 
loss_bc      <- clean_input(control_file[grep('create_loss_bc',  control_file)]) 
create_fvsed <- clean_input(control_file[grep('create_fvsed',    control_file)]) 
create_fvc   <- clean_input(control_file[grep('create_fvc',      control_file)]) 
run_tfv      <- clean_input(control_file[grep('run_tfv',         control_file)]) 
proc_results <- clean_input(control_file[grep('process_results', control_file)]) 
# Read in batch file template
create_batch <- readLines(paste0(path, '000_ADM/run_model.tmp'))
# Turn components on/off based on control file
if (run_HS == 0) {
  create_batch[grep('001_adjust_landcodes.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('001_adjust_landcodes.R', create_batch)])
  create_batch[grep('002_create_hs_batch.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('002_create_hs_batch.R', create_batch)])
  create_batch[grep('run_heatsource.bat', create_batch)] <- 
    paste0('REM ', create_batch[grep('run_heatsource.bat', create_batch)])
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
if (run_tfv == 0) {
  create_batch[grep('TUFLOWFV', create_batch)] <- 
    paste0('REM ', create_batch[grep('TUFLOWFV', create_batch)],
           ' 002_TFV\\cf\\', run_name, '.fvc')
}
if (proc_results == 0) {
  create_batch[grep('Process_fv_out.R', create_batch)] <- 
    paste0('REM ', create_batch[grep('Process_fv_out.R', create_batch)])
}
writeLines(text = create_batch, con = paste0(path, 'run_model.bat'))








