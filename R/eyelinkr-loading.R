#' Loads in sr 1000 asc file and outputs a list
#'
#' @param filepath path to the asc file
#'
#' @return
#' @export
#'
#' @examples
load_asc_file <- function(filepath){
  text <- readLines(filepath)
  ls <- list()
  #ls$gaze <- parse_gaze(text)
  ls$fixations <- parse_fixations(text)
  ls$events <- parse_events(text)
  #ls$calibration <- parse_calibrations(text)
  return(ls)
}

#' Reads eye fixations from a asc file
#'
#' @param filepath Path to the asc file
#'
#' @return
#' @export
#'
#' @examples
read_fixations <- function(filepath){
  text <- readLines(filepath)
  df <- parse_fixations(text)
  return(df)
}

#' Decodes fixations from the given character vector
#'
#' @description Used in case you load the text elsewhere or you just
#' want to decode parts of it. In case you want to parse entire file, you can
#' use read_fixations instead
#'
#' @param text
#'
#' @return
#' @export
#'
#' @examples
parse_fixations <- function(text){
  FIX_idxs <- grep('^EFIX.*', text)
  lines <- text[FIX_idxs]

  #Replaces all the EFIX R/L part up to the number
  lines <- gsub('^EFIX R\\s+', '', lines, perl = T)
  lines <- gsub('^EFIX L\\s+', '', lines, perl = T)

  #creates one file with each char on a single line
  text <- paste(lines, sep = "", collapse="\n")
  df <- read.table(text = text, sep = "\t", header = F)
  colnames(df) <- c("start", "end", "no_idea_1", "x", "y", "no_idea_2")
  df$duration <- df$end - df$start
  return(df)
}

#' Reads in events from the given filepath
#'
#' @param filepath path to the asc file
#'
#' @return data.frame with the events
#' @export
#'
#' @examples
read_events <- function(filepath){
  text <- readLines(filepath)
  tab <- parse_events(text)
  return(tab)
}

#' Decodes events from passed sr1000 text file
#'
#' @description Used in case you load the text elsewhere or you just
#' want to decode parts of it. In case you want to parse entire file, you can
#' use read_events instead
#'
#' @param text
#'
#' @return data.frame
#' @export
#'
#' @examples
parse_events <- function(text){
  EVENT_NAMES <- c("KEY_UP", "KEY_DOWN")
  i_msg <- grep('^MSG\\t+.*', text)
  lines <- text[i_msg]
  i_events <- sapply(lines, contains_word, EVENT_NAMES)
  lines <- lines[i_events]
  #removing the MSG part
  lines <- gsub('^MSG\t', '', lines, perl = T)
  #creates one file with each char on a single line
  text <- paste(lines, sep = "", collapse = "\n")
  tab <- read.table(text = text, sep = " ", header = F)
  tab[, c("V2", "V4", "V5", "V6")] <- NULL
  colnames(tab) <- c("time", "name", "type")
  return(tab)
}

#' Read gaze data from the asc
#'
#' @param filepath path to the asc file
#'
#' @return
#' @export
#'
#' @examples
read_gaze <- function(filepath){
  text <- readLines(filepath)
  df <- parse_gaze(text)
  return(df)
}

#' Decodes gaze information from passed parsed asc text
#'
#' @description Used in case you load the text elsewhere or you just
#' want to decode parts of it. In case you want to parse entire file, you can
#' use read_gaze instead
#'
#' @param text character vector of the asc file
#'
#' @return data.frame with the gaze
#' @export
#'
#' @examples
parse_gaze <- function(text){
  DATA_indexes <- grep("^[0-9]+.*$", text)
  pseudo_file <- paste(text[DATA_indexes], collapse = "\n")
  df <- read.table(text = pseudo_file, header = F, col.names = c("frame", "x", "y", "pupil", "no_idea", "some_dots"))
  df$x <- as.double(df$x)
  df$y <- as.double(df$y)
  return(df)
}

#' Reads in calibration
#'
#' @param text character vector of the asc file
#'
#' @return
#' @export
#'
#' @examples
parse_calibrations <- function(text){
  #will produce empty lines in the calibration
  calibrations <- data.frame(
    calib.time  = numeric(0),
    trial       = numeric(0),
    eye         = character(0),
    rating      = character(0),
    error.avg   = numeric(0),
    error.max   = numeric(0),
    stringsAsFactors = F)
  for (line in text)
    if (grepl("!CAL VALIDATION", line) & !grepl("ABORTED", line)) {
      msg <- unlist(strsplit(line, "[\t ]"))
      ls <- list(calib.time = etime, trial = current.trial,
                 eye = msg[7], rating = msg[8],
                 error.avg = as.numeric(msg[10]),
                 error.max = as.numeric(msg[12]))
      calibrations <- rbind(calibrations, ls)
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
