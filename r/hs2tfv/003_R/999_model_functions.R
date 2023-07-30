# MD-WERP 9.2 Water Quality Modelling; Tuflow FV WQ 
# Ryan Shojinaga & David Hamilton, Australian Rivers Institute, Griffith Uni.
# ryan.shojinaga@griffithuni.edu.au

# Functions to support the Heatsource-Tuflow FV model framework
clean_input <- function(line) {
  # Takes a line from the control file and returns the value (removes other 
  # info); detect if date or numeric
  type <- ifelse(grepl('date', line), 'date', 'othr')
  find <- unlist(gregexpr('== ', line))
  line <- substr(line, find + 2, stop = nchar(line))
  line <- gsub('\\s+', '', line) # remove white space
  if (type == 'date') {
    # Dates must be in format of YYYY-MM-DD
    line <- as.POSIXct(line, '%Y-%m-%d', tz = 'Australia/Brisbane')
  } else if (suppressWarnings(!is.na(as.numeric(line)))) {
    line <- as.numeric(line)
  }
  return(line)
}

convert_2_date <- function(year, month, day) {
  as.POSIXct(paste0(as.character(year), '-', as.character(month), '-', 
                    as.character(day)), '%Y-%m-%d', tz = 'Australia/Brisbane')
}

write_scripts <- function(path) {
  temp_files <- list.files(paste0(path, '003_R/tmpl/'))
  path_with_quotes <- paste0("\'", path, "\'")
  for (i in 1 : length(temp_files)) {
    temp_script <- readLines(paste0(path, '003_R/tmpl/', temp_files[i]))
    temp_script <- gsub('XX_PATH_XX', path_with_quotes, temp_script)
    new_R_file  <- gsub('.tmpl', '.R', temp_files[i])
    writeLines(text = temp_script, con = paste0(path, '003_R/', new_R_file))
  }
}

