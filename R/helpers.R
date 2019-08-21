contains_word <-function(ls, words){
  #basically iterates through list and sees if at least one of the columns returns true
  for(word in words){
    if (sum(grepl(word, ls)) > 0) return(TRUE)
  }
  return(FALSE)
}
