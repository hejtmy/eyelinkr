# SR 1000 Eyelink eyetracker functions
read_eye_fixations <- function(text){
  FIX_idxs <- grep('^EFIX.*', text)
  lines <- text[FIX_idxs]

  #Replaces all the EFIX R/L part up to the number
  lines <- gsub('^EFIX R\\s+', '', lines, perl = T)
  lines <- gsub('^EFIX L\\s+', '', lines, perl = T)

  #creates one file with each char on a single line
  text <- paste(lines, sep="", collapse="\n")
  tab <- read.table(text = text, sep = "\t", header = F)
  colnames(tab) <- c("start", "end", "no_idea_1", "x", "y", "no_idea_2")
  tab$duration <- tab$end - tab$start
  return(tab)
}

read_eye_events <- function(text){
  EVENT_NAMES <- c("KEY_UP", "KEY_DOWN")
  i_msg <- grep('^MSG\\t+.*', text)
  lines <- text[i_msg]
  i_events <- sapply(lines, contains_word, EVENT_NAMES)
  lines <- lines[i_events]
  #removing the MSG part
  lines = gsub('^MSG\t', '', lines, perl = T)
  #creates one file with each char on a single line
  text <- paste(lines, sep = "", collapse = "\n")
  tab <- read.table(text = text, sep = " ", header = F)
  tab[, c("V2", "V4", "V5", "V6")] <- NULL
  colnames(tab) <- c("time", "name", "type")
  return(tab)
}

read_eye_movements <- function(text){
  DATA_indexes <- grep("^[0-9]+.*$", text)
  pseudo_file <- paste(text[DATA_indexes], collapse="\n")
  dat <- read.table(text = pseudo_file, header = F, col.names = c("frame", "x", "y", "pupil", "no_idea", "some_dots"))
  dat$x <- as.double(dat$x)
  dat$y <- as.double(dat$y)
  return(dat)
}

read_calibrations <- function(text, ncal){
  #will produce empty lines in the calibration
  calibrations <- data.frame(
    calib.time  = numeric(n.calibrations),
    trial       = numeric(n.calibrations),
    eye         = character(n.calibrations),
    rating      = character(n.calibrations),
    error.avg   = numeric(n.calibrations),
    error.max   = numeric(n.calibrations),
    stringsAsFactors = F)
  ncal <- 0
  for (line in text)
    if (grepl("!CAL VALIDATION", line) &
        !grepl("ABORTED", line)) {
      msg <- unlist(strsplit(line, "[\t ]"))
      ncal <- ncal + 1
      v.eye    <- msg[7]
      v.rating <- msg[8]
      v.error.avg <- as.numeric(msg[10])
      v.error.max <- as.numeric(msg[12])
      calibrations$calib.time[ncal]  <- etime
      calibrations$trial[ncal]  <- current.trial
      calibrations$eye[ncal]    <- v.eye
      calibrations$rating[ncal] <- v.rating
      calibrations$error.avg[ncal] <- v.error.avg
      calibrations$error.max[ncal] <- v.error.max
    }
  return(calibrations)
}

read_which_eye <- function(filepath){
  #Starts reading the file
  con <- file(filepath, 'r');
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    # SEARCHES FOR THE START INFORMATION
    # which eye we will record?
    if (grepl("^START", oneLine)) {
      eye <- "unknown"
      if (grepl("LEFT", oneLine)) {
        eye <- "left"
      }
      if (grepl("RIGHT", oneLine)) {
        if (eye == "left") {
          eye <- "both"
        } else {
          eye <- "right"
        }
      }
      close(con)
      return(eye)
    }
  }
  close(con)
  return("unknown")
}

# goes through the asc log and finds display options
read_resolution <- function(filepath){
  con <- file(filepath, open = "r")
  disp_resolution <- NULL
  # Needs <- assign becasue it doesn't work otherwise in the length function
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    # EXAMPLE = MSG	21256557 DISPLAY_COORDS 0 0 1919 1079
    if(grepl("DISPLAY_COORDS", line)){
      #' match two digits at least three digit long after Display coords
      #' ? signifies non greedy match (as least as possible)
      ptr = ".*DISPLAY_COORDS.*?(\\d{3,})\\s*(\\d{3,})"
      disp_resolution = gsub(ptr, "\\1;\\2", line)
      sep = strsplit(disp_resolution, ";")
      width = as.numeric(sep[[1]][1])
      height = as.numeric(sep[[1]][2])
      width = ceiling(width/10)*10
      height = ceiling(height/10)*10
      disp_resolution = (list("width" = width, "height" = height))
      break
    }
  }
  close(con)
  return(disp_resolution)
}
