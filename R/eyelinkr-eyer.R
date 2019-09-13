#' Converts loaded data to EyerObject
#' @details the data is a list with fixations, gaze and events fields. It converts them to the
#' Eyer object structure and does little preprocessing
#'
#' @description The EyerObject is a default eyetracking object to be used with the `eyer` package.
#' It is recommended that you preprocess your data before converting it to eyer, as it might be more
#' difficult to translate some data, especially events
#'
#' @param obj list loaded by \code{\link{load_asc}} and preprocessed
#'
#' @return EyerObject
#' @export
#'
#' @examples
convert_to_eyer <- function(obj){
  # validations
  REQUIRED_DATA_FILEDS <- c("gaze", "fixations")
  if(!all(REQUIRED_DATA_FILEDS %in% names(obj$data))){
    warning("Data don't include all required fields", REQUIRED_DATA_FILEDS)
    return(obj)
  }
  ls <- list(info=obj$info)
  gaze <- obj$data$gaze

  ls$info$start_time <- gaze$timestamp[1]
  gaze$time <- (gaze$timestamp - ls$info$start_time)/ls$info$frequency
  gaze <- remove_columns(gaze, c("timestamp", "some_dots"))

  fixations <- obj$data$fixations
  fixations$time <- (fixations$start - ls$info$start_time)/ls$info$frequency
  fixations <- remove_columns(fixations, c("start", "no_idea", "end"))

  events <- obj$data$events
  events$timestamp <- (events$timestamp - ls$info$start_time)/ls$info$frequency
  events <- rename_column(events, "timestamp", "time")

  ls$data <- list(events = events, fixations = fixations, gaze = gaze)
  class(ls) <- append(class(ls), "eyer")
  return(ls)
}
