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
  df$name <- sapply(df$name, rm_brackets)
  return(df)
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
  df <- df[df$action == "KEY_UP", ]
  df$action <- factor(df$action) #refactors after removing one level
  return(df)
}

#' Helper to remove keys which are used for walking in 3D games
#'
#' @description basically just a shorthand for `remove_event_keys(events, c('w','a','s','d','UP','BACK', 'LEFT', 'RIGHT', 'DOWN'))`
#'
#' @param df event data.frame as loaded by `read_events` or `parse_events`
#'
#' @return
#' @export
#'
#' @examples
remove_walking_keys <- function(df){
  df <- remove_event_keys(df, c('w','a','s','d','UP','BACK', 'LEFT', 'RIGHT', 'DOWN'))
  df <- remove_event_keys(df, c('[w]','[a]','[s]','[d]','[UP]','[BACK]','[LEFT]','[RIGHT]', '[DOWN]'))
  return(df)
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
remove_event_keys <- function(df, keys){
  df <- df[!(df$name %in% keys), ]
  return(df)
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
