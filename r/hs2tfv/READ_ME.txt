
OPEN 000_create_model_run.R

Hardcode path into script as follows:

# FOLDER PATH HERE (Remeber to replace '\' with '/')____________________________
path <- MODEL_RUN_FOLDER_PATH
#_______________________________________________________________________________


RUN start_run.bat

RUN run_model.bat



000_create_model_run does the following:
- Creates R scripts with the static folder path (top level) for the  model run
- Creates the master batch file with user-specfied model components included