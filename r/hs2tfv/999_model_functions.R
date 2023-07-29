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
