contains_word <-function(ls, words){
  #basically iterates through list and sees if at least one of the columns returns true
  for(word in words){
    if (sum(grepl(word, ls)) > 0) return(TRUE)
  }
  return(FALSE)
}

rename_column <- function(df, old_column, new_column){
  colnames(df)[old_column == colnames(df)] <- new_column
  return(df)
}

remove_columns <- function(df, column_names){
  i_cols <- which(colnames(df) %in% column_names)
  df[, i_cols] <- NULL
  return(df)
}
