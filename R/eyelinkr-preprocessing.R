#' Removes brackets from the parsed events
#'
#' @param df event data.frame as loaded by `read_events` or `parse_events`
#'
#' @return
#' @export
#'
#' @examples
remove_event_brackets <- function(df){
  rm_brackets <- function(x) gsub("\\[|\\]", "", x)
  df_events$type <- sapply(df_events$type, rm_brackets)
  return(df_events)
}

#' Helper to remove all depression of keys
#'
#' @description Eyelink events log both key down and key_up events.
#' these are for many purposes unnecessary, so we can half the number
#' of logged events by removing key up (user releases key) events
#'
#'
#' @param df event data.frame as loaded by `read_events` or `parse_events`
#'
#' @return
#' @export
#'
#' @examples
remove_key_up <- function(df){
  df <- df[df$name == "KEY_UP", ]
  return(df)
}

#' Helper to remove keys which are used for walking in 3D games
#'
#' @description basically just a shorthand for `remove_event_keys(events, c('w','a','s','d','UP','BACK', 'LEFT', 'RIGHT'))`
#'
#' @param df_events df event data.frame as loaded by `read_events` or `parse_events`
#'
#' @return
#' @export
#'
#' @examples
remove_walking_keys <- function(df_events){
  df_events <- remove_event_keys(df_events, c('w','a','s','d','UP','BACK', 'LEFT', 'RIGHT'))
  df_events <- remove_event_keys(df_events, c('[w]','[a]','[s]','[d]','[UP]','[BACK]','[LEFT]','[RIGHT]'))
  return(df_events)
}

#' Removes events with unwanted keys
#'
#' @param df_events df event data.frame as loaded by `read_events` or `parse_events`
#' @param keys character vector of all the keys you want to remove. e.g c("w", "a"). If you didn't
#' use the remove bracket function, these need to be in the [w] format
#'
#' @return
#' @export
#'
#' @examples
remove_event_keys <- function(df_events, keys){
  df_events <- df_events[!(df_events$type %in% keys), ]
  return(df_events)
}

#' Flips the Y axis, which is logged with the 0 in bottom left corner
#'
#' @param df dataframe with x, y columns, either gaze or fixations
#' @param disp_resolution list with height and width fields
#'
#' @return data.frame with y axis flipped
#' @export
#'
#' @examples
flip_y_axis <- function(df, disp_resolution){
  df$y <- disp_resolution$height - df$y
  return(df)
}
