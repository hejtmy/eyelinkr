preprocess_eye_events <- function(df_events){
  ls <- list()
  df_events <- SR1000.remove_brackets(df_events)
  df_events <- SR1000.remove_key_up(df_events)
  df_events <- SR1000.remove_walking_keys(df_events)
  df_events <- SR1000.remove_event_keys(df_events, ';')
  return(df_events)
}

remove_brackets <- function(df_events){
  rm_brackets <- function(x) gsub("\\[|\\]", "", x)
  df_events$type <- sapply(df_events$type, rm_brackets)
  return(df_events)
}

remove_key_up <- function(df_events){
  df_events <- df_events[df_events$name == "KEY_UP",]
  return(df_events)
}

remove_walking_keys <- function(df_events){
  df_events <- SR1000.remove_event_keys(df_events, c('w','a','s','d', 'UP', 'BACK'))
  return(df_events)
}

remove_event_keys <- function(df_events, keys){
  df_events <- df_events[!(df_events$type %in% keys),]
  return(df_events)
}

preprocess_eye_fixations <- function(df_fixations){
  df_fixations$position_y <- disp_resolution$height - df_fixations$positions_y
  return(df_fixations)
}
