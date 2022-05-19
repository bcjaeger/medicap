

is_double_ish <- function(x){

  out <- is.numeric(x) && any(as.integer(x) != x)

  if(is.na(out)) return(FALSE)

  out

}
